#' ptaPred UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats rnorm
#' @import ggplot2

mod_ptaPred_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "utilitary.css")
    ),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          box(
            width = 12,
            status = "olive",
            solidHeader = TRUE,
            title = "Information sur le Traitement",
            selectInput(ns("beta_lactamin"), label = labels("drug", "label", lang), choices = labels("drug", "choices", lang), selected = character(0), width = "auto"),
            selectInput(ns("administration_route"), labels("administration_route", "label", lang), choices = labels("administration_route", "choices", lang), selected = "IV", width = "auto"),
            numericInput(ns("drug_dose"), label = labels("dose_input", "label", lang), value = 1000, step = 0.125, min = 0, max = 32, width = "auto"),
            br(),
            selectInput(ns("bacteria_select"), "Selectionner Bacterie", choices = c("Traitement Probabiliste" = "probabilist", "other"), selected = "probabilist", width = "auto")
          ),
          box(
            width = 12,
            status = "lightblue",
            solidHeader = TRUE,
            title = "Information Patient",
            numericInput(ns("age"), label = labels("age", "label", lang), value = 18, min = 0, max = 1000, step = 1),
            numericInput(ns("height"), label = labels("height", "label", lang), value = 180, min = 0, max = 1000, step = 1),
            numericInput(ns("weight"), label = labels("weight", "label", lang), value = 70, min = 0, max = 1000, step = 1),
            numericInput(ns("creatinine"), label = labels("creatinine", "label", lang), value = 60, min = 0, max = 1000, step = 1),
            selectInput(ns("sex"), label = labels("sex", "label", lang), choices = labels("sex", "choices", lang), selected = "Male")
            # choice ethnicity
            # add all patient info to be computed in pop pk model (no bayesian?)
          )
        ),
        column(
          width = 6,
          box(
            width = 12,
            background = "secondary",
            collapsible = FALSE,
            box(
              width = 12,
              solidHeader = FALSE,
              title = "PTA output",
              plotOutput(ns("pta_output"))
            ),
            box(
              width = 12,
              solidHeader = FALSE,
              title = "PTA Probability output",
              plotOutput(ns("pta_output_probability"))
            ),
            footer = fluidRow(
              col_2(actionButton(ns("compute_pta"), "Generer les PTA", style = "background-color: #3d9970; color: white;")),
              col_4(
                selectInput(ns("css_mic_target"), label = labels("target", "label", lang), choices = labels("target", "choices", lang), selected = "one_mic"),
                offset = 1
              ),
              col_3(
                offset = 1,
                sliderInput(ns("confidence_level"), label = labels("conf_interval", "label", lang), min = 0, max = 1, value = c(0.025, 0.975), step = 0.01)
              )
            )
          )
        ),
        column(
          width = 3,
          tagList(
            div(
              class = "information-panel pull-right",
              p("Information Patient", style = "font-weight: bold; font-size: 18px;"),
              htmlOutput(outputId = "patient_information")
            )
          ),
          tagList(
            div(
              class = "disclamer-panel pull-right",
              p("Disclamer", style = "font-weight: bold; font-size: 16px;"),
              p("1. Aide a la decision"),
              p("2. ne prend pas en compte l ecologie locale"),
              p("3. regarder le modele sous jacent (defaut ICU) mais specificite des modeles decrites dans longlet model")
            )
          )
        )
      )
    )
  )
}

#' ptaPred Server Functions
#'
#' @noRd
mod_ptaPred_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$compute_pta, {
      # general info
      creat_unit <- "uM/L"
      weight_unit <- "kg"

      # all weight calculated
      tbw <- input$weight
      lbw <- weight_formula(input$weight, input$height, input$sex, weight_unit, formula = "LBW")
      ajbw <- weight_formula(input$weight, input$height, input$sex, weight_unit, formula = "AJBW")
      ibw <- weight_formula(input$weight, input$height, input$sex, weight_unit, formula = "IBW")
      bmi <- round(input$weight / (input$height / 100) ^ 2, digits = 1)
      bsa <- bsa(input$height, input$weight)

      # calculate all clearance
      cg_tbw <- renal_function(input$sex, input$age, tbw, input$height, input$creatinine, formula = "CG", creat_unit = creat_unit)
      cg_ajbw <- renal_function(input$sex, input$age, ajbw, input$height, input$creatinine, formula = "CG", creat_unit = creat_unit)
      cg_ibw <- renal_function(input$sex, input$age, ibw, input$height, input$creatinine, formula = "CG", creat_unit = creat_unit)
      cg_lbw <- renal_function(input$sex, input$age, lbw, input$height, input$creatinine, formula = "CG", creat_unit = creat_unit)
      mdrd <- renal_function(input$sex, input$age, tbw, input$height, input$creatinine, formula = "MDRD", creat_unit = creat_unit)
      ckd_2009 <- renal_function(input$sex, input$age, tbw, input$height, input$creatinine, formula = "CKD_2009", creat_unit = creat_unit)
      ckd_2021 <- renal_function(input$sex, input$age, tbw, input$height, input$creatinine, formula = "CKD_2021", creat_unit = creat_unit)
      schwartz <- renal_function(input$sex, input$age, tbw, input$height, input$creatinine, formula = "schwartz", creat_unit = creat_unit)

      # model_parameters
      tvcl <- 0.526 + 2 * cg_lbw / 54
      eta_cl <- 0.293
      tv_css <- input$drug_dose / (tvcl * 24) # typical css
      mic <- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64) # to change
      css_mic <- tv_css / mic


      # calculate css
      set.seed(16897)
      cl_distribution <- stats::rnorm(100000, mean = tvcl, sd = eta_cl)
      css_distribution <-  input$drug_dose / (cl_distribution * 24)
      q <- stats::quantile(css_distribution, probs = c(0.025, 0.975))
      #css over mic distribution

      #create datagframe with all value
      concentration_df <- data.frame(
        CSS_MIC = css_mic,
        MIC = mic,
        perc_2.5 = q[1] / mic,
        perc_97.5 = q[2] / mic
      )

      # plot pta with css/mic
      output$pta_output <- renderPlot({
        ggplot(data = concentration_df, aes(x = .data$MIC, y = .data$CSS_MIC)) +
          geom_line(col = "#2db391", lty = 1, lwd = 1) +
          geom_hline(aes(yintercept = 8, linetype = "ECOFF"), lwd = 1, col = "red") + # to modify by EUCAST ECOFF for a given bacteria
          geom_hline(aes(yintercept = 102, linetype = "Toxicity Levels"), lwd = 1, col = "#960b0b") +
          scale_linetype_manual(
            name = "Breakpoint",
            values = c(2, 2),
            guide = guide_legend(override.aes = list(color = c("#960b0b", "red")))
          ) +
          labs(linetype = NULL) +
          scale_x_log10(breaks = mic, labels = mic) +
          scale_y_log10() +
          xlab("MIC (mg/L)") +
          ylab("Css/MIC") +
          theme_classic(base_size = 14) +
          theme(
            legend.position = "inside",
            legend.justification.inside = c(0.9, 0.9),
            legend.box.background = element_rect()
          )
      })

      # plot pta with css/mic probability quantile based on user selection
      output$pta_output_probability <- renderPlot({
        ggplot(data = concentration_df, aes(x = .data$MIC, y = .data$CSS_MIC)) +
          geom_line(col = "#2db391", lty = 1, lwd = 1) +
          geom_hline(aes(yintercept = 8, linetype = "ECOFF"), lwd = 1, col = "red") + # to modify by EUCAST ECOFF for a given bacteria
          geom_hline(aes(yintercept = 102, linetype = "Toxicity Levels"), lwd = 1, col = "#960b0b") +
          scale_linetype_manual(
            name = "Breakpoint",
            values = c(2, 2),
            guide = guide_legend(override.aes = list(color = c("#960b0b", "red")))
          ) +
          labs(linetype = NULL) +
          scale_x_log10(breaks = mic, labels = mic) +
          scale_y_log10() +
          xlab("MIC (mg/L)") +
          ylab("Css/MIC") +
          theme_classic(base_size = 14) +
          theme(
            legend.position = "inside",
            legend.justification.inside = c(0.9, 0.9),
            legend.box.background = element_rect()
          ) +
          geom_line(data = concentration_df, aes(y = .data$perc_2.5, x = .data$MIC), col = "#0889f1", lty = 2) +
          geom_line(data = concentration_df, aes(y = .data$perc_97.5, x = .data$MIC), col = "#0889f1", lty = 2)
      })
    })

  })
}

## To be copied in the UI
# mod_ptaPred_ui("ptaPred_1")

## To be copied in the server
# mod_ptaPred_server("ptaPred_1")
