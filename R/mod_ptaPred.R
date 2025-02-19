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
#' @import plotly

mod_ptaPred_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "utilitary.css")
    ),
    fluidPage(
      fluidRow(
        column(
          width = 2,
          box(
            width = 12,
            status = "olive",
            solidHeader = TRUE,
            title = "Information sur le Traitement",
            selectInput(ns("beta_lactamin"), label = labels("drug", "label", lang), choices = labels("drug", "choices", lang), selected = character(0), width = "auto"),
            numericInput(ns("drug_dose"), label = labels("dose_input", "label", lang), value = 0, step = 0.125, min = 0, max = 32, width = "auto"),
            br(),
            selectInput(ns("bacteria_select"), "Selectionner Bacterie", choices = c("Traitement Probabiliste" = "probabilist", "other"), selected = "probabilist", width = "auto")
          ),
          box(
            width = 12,
            status = "lightblue",
            solidHeader = TRUE,
            title = "Information Patient",
            numericInput(ns("age"), label = labels("age", "label", lang), value = 18, min = 0, max = 120, step = 1),
            numericInput(ns("height"), label = labels("height", "label", lang), value = 180, min = 0, max = 250, step = 1),
            numericInput(ns("weight"), label = labels("weight", "label", lang), value = 70, min = 0, max = 500, step = 1),
            numericInput(ns("creatinine"), label = labels("creatinine", "label", lang), value = 60, min = 0, max = 1500, step = 1),
            selectInput(ns("sex"), label = labels("sex", "label", lang), choices = labels("sex", "choices", lang), selected = "Male")
            # choice ethnicity
            # add all patient info to be computed in pop pk model (no bayesian?)
          )
        ),
        column(
          width = 7,
          offset = 1,
          box(
            width = 12,
            background = "secondary",
            collapsible = FALSE,
            box(
              width = 12,
              solidHeader = FALSE,
              title = "PTA output",
              plotlyOutput(ns("pta_output"))
            ),
            box(
              width = 12,
              solidHeader = FALSE,
              title = "PTA Probability output",
              plotlyOutput(ns("pta_output_probability"))
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

    
    if (Sys.getenv("PRODUCTION_MODE") == "TRUE") {
      # modal open on app launch to warn people
      observe({
      showModal(
        modalDialog(
        size = "xl",
        div(
          class = "disclamer-panel pull-right",
          p("Disclamer", style = "font-weight: bold; font-size: 16px; text-align: center;"),
          p("1. Aide a la decision"),
          p("2. ne prend pas en compte l ecologie locale"),
          p("3. regarder le modele sous jacent (defaut ICU) mais specificite des modeles decrites dans longlet model")
        ),
        easyClose = FALSE,
        modalButton("Accept"),
        footer = NULL
        )
      )
      })
    }

    # PTA computing and plotting code
    observeEvent(input$compute_pta, {
      # general info
      biological <- calc_biological(
        weight = input$weight,
        height = input$height,
        sex = input$sex,
        age = input$age,
        creatinine = input$creatinine,
        weight_unit = "kg",
        creat_unit = "mg/dL"
      )
      # calculate model parameters (cl and eta_cl) based on selected drug
      model_param <- get_model_parameters("klastrup_2020", biological = biological, drug = input$beta_lactamin)

      # calculate all concentration
      concentration_df <- sim_concentration(
        dose = input$drug_dose,
        tvcl = model_param$cl,
        eta_cl = model_param$eta_cl,
        quantile = input$confidence_level,
        dose_increment = model_param$dose_increment
      )


      pta_plot <- ggplot(data = concentration_df, aes(x = .data$mic, y = .data$css_mic)) +
        geom_line(col = "#2db391", lty = 1, lwd = 1) +
        geom_hline(aes(yintercept = 102, linetype = "Toxicity Levels"), lwd = 1, col = "#960b0b") +
        geom_hline(aes(yintercept = 8, linetype = "Efficay Threshold"), lwd = 1, col = "red") + # to modify by EUCAST ECOFF for a given bacteria
        geom_vline(aes(xintercept = 8, linetype = "ECOFF"), lwd = 1, col = "black") +
        scale_linetype_manual(
          name = "Breakpoint",
          values = c(2, 2, 2),
          guide = guide_legend(override.aes = list(color = c("#960b0b", "red", "black")))
        ) +
        labs(linetype = NULL) +
        scale_x_log10(breaks = concentration_df$mic, labels = concentration_df$mic, limits = c(max(0.01, min(concentration_df$mic)), max(concentration_df$mic))) +
        scale_y_log10(limits = c(max(0.01, min(concentration_df$css_mic_below2)), max(concentration_df$css_mic_above2))) +
        xlab("MIC (mg/L)") +
        ylab("Css/MIC") +
        theme_classic(base_size = 14) +
        theme(
          legend.position = "inside",
          legend.justification.inside = c(0.9, 0.9),
          legend.box.background = element_rect()
        )

      pta_multiple_doses <- pta_plot +
        geom_line(data = concentration_df, mapping = aes(x = .data$mic, y = .data$css_mic_below1), col = "#20846b", lty = 1, lwd = 1) +
        geom_line(data = concentration_df, mapping = aes(x = .data$mic, y = .data$css_mic_below2), col = "#1f8269", lty = 1, lwd = 1) +
        geom_line(data = concentration_df, mapping = aes(x = .data$mic, y = .data$css_mic_above1), col = "#32c5a0", lty = 1, lwd = 1) +
        geom_line(data = concentration_df, mapping = aes(x = .data$mic, y = .data$css_mic_above2), col = "#2fe3b6", lty = 1, lwd = 1)

      pta_ci_plot <- pta_plot +
        geom_ribbon(data = concentration_df, aes(ymin = .data$percentile_2.5, ymax = .data$percentile_97.5, x = .data$mic), fill = "#0889f1", alpha = 0.1, col = "#0889f1")

      # plot pta with css/mic
      output$pta_output <- renderPlotly({ plotly::ggplotly(pta_multiple_doses) })

      # plot pta with css/mic probability quantile based on user selection
      output$pta_output_probability <- renderPlotly({ plotly::ggplotly(pta_ci_plot) })
    })
  })
}

## To be copied in the UI
# mod_ptaPred_ui("ptaPred_1")

## To be copied in the server
# mod_ptaPred_server("ptaPred_1")
