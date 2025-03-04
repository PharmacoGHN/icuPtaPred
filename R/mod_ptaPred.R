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
#' @import shinyvalidate

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
            status = "lightblue",
            solidHeader = TRUE,
            title = "Information Patient",
            numericInput(ns("age"), label = labels("age", "label", lang), value = 18, min = 0, max = 120, step = 1),
            numericInput(ns("height"), label = labels("height", "label", lang), value = 180, min = 0, max = 250, step = 1),
            numericInput(ns("weight"), label = labels("weight", "label", lang), value = 70, min = 0, max = 500, step = 1),
            numericInput(ns("creatinine"), label = labels("creatinine", "label", lang), value = 60, min = 0, max = 1500, step = 1),
            selectInput(ns("creatinine_unit"), label = "Creatinine Unit", choices = c("mg/dL" = "mg/dL", "µmol/L" = "uM/L"), selected = "mg/dL"),
            # numericInput(ns("cystatin_c"), label = labels("cystatin_c", "label", lang), value = 0, min = 0, max = 1500, step = 1),
            numericInput(ns("urine_output"), label = labels("urinary_output", "label", lang), value = 1500, min = 0, max = 5000, step = 1),
            numericInput(ns("urine_creatinine"), label = labels("urinary_creat", "label", lang), value = 0, min = 0, max = 1500, step = 1),
            selectInput(ns("sex"), label = labels("sex", "label", lang), choices = labels("sex", "choices", lang), selected = "Male")
            # choice ethnicity
            # add all patient info to be computed in pop pk model (no bayesian?)
          )
        ),
        column(
          width = 8,
          column(
            width = 12,
            box(
              width = 12,
              solidHeader = TRUE,
              status = "olive",
              collapsible = FALSE,
              title = "PTA output",
              plotlyOutput(ns("pta_output")),
              footer = uiOutput(ns("footer_pta"))
            ),
            box(
              width = 12,
              solidHeader = TRUE,
              status = "olive",
              collapsible = FALSE,
              title = "PTA Probability output",
              plotlyOutput(ns("pta_output_probability")),
              footer = uiOutput(ns("footer_pta_probability"))
            )
          )
        ),
        column(
          width = 2,
          box(
            width = 12,
            status = "olive",
            solidHeader = TRUE,
            title = "Information sur le Traitement",
            selectInput(ns("bacteria_select"), "Selectionner Bacterie", choices = "probabilist", selected = "probabilist", width = "auto"),
            selectInput(ns("beta_lactamin"), label = labels("drug", "label", lang), choices = labels("drug", "choices", lang), selected = character(0), width = "auto"),
            selectInput(ns("model_selected"), label = "Select Model:", choices = character(0), width = "auto"),
            uiOutput(ns("model_choice")),
            numericInput(ns("drug_dose"), label = labels("dose_input", "label", lang), value = 0, step = 0.125, min = 0, max = 32, width = "auto")
          ),
          rep_br(2),
          selectInput(ns("css_mic_target"), label = labels("target", "label", lang), choices = labels("target", "choices", lang), selected = "one_mic"),
          sliderInput(ns("confidence_level"), label = labels("conf_interval", "label", lang), min = 0, max = 1, value = c(0.025, 0.975), step = 0.01),
          rep_br(2),
          actionButton(ns("compute_pta"), "Compute PTA", style = "background-color: #3d9970; color: white; border-color: black;"),
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

    # create reactive value to store mic data
    mic_information <- reactiveVal()
    mic_specie <- reactiveVal()
    ecoff <- reactiveVal()
    ecoff_ci <- reactiveVal()

    # [Validator] _______________________________________________
    validator <- InputValidator$new()
    validator$add_rule("drug_dose", function(value) { if (value == 0) "Dose must be greater than 0"})
    validator$add_rule("height", function(value) { if (value < 10) "Height must be in cm"})
    validator$add_rule("height", function(value) { if (value > 250) "Height must be less than 250 cm"})
    validator$add_rule("weight", function(value) { if (value < 1) "Weight must be in kg"})
    validator$add_rule("weight", function(value) { if (value > 500) "Weight must be less than 500 kg"})
    validator$add_rule("age", function(value) { if (value <= 0) "Age must be greater than 0"})
    validator$add_rule("age", function(value) { if (value > 120) "Age must be less than 120"})
    validator$add_rule("model_selected", function(value) { if (value == "Barreto_2023") "Not currently supported"})
    validator$add_rule("model_selected", function(value) { if (value == "Gijsen_2021") "Not currently supported"})
    validator$add_rule("model_selected", function(value) { if (value == "Minichmayr_2018") "Not currently supported"})
    validator$add_rule("model_selected", function(value) { if (value == "Ehrmann_2019") "Not currently supported"})
    validator$add_rule("model_selected", function(value) { if (value == "Huang_2025") "Not currently supported"})
    validator$add_rule("model_selected", function(value) { if (value == "Lan_2022") "Not currently supported"})

    validator$enable()

    # [Model Choice] _______________________________________________
    observeEvent(input$beta_lactamin, {
      updateSelectInput(session, "model_selected", choices = names(model_information[[input$beta_lactamin]]))
    })


    # create the warning message to display on launch
    # [Warning - Disclamer] ______________________________________
    warning_message <- div(
      class = "disclamer-panel pull-right",
      p("Disclamer", style = "font-weight: bold; font-size: 16px; text-align: center;"),
      p("1. Aide a la decision"),
      p("2. ne prend pas en compte l ecologie locale"),
      p("3. regarder le modele sous jacent (defaut ICU) mais specificite des modeles decrites dans longlet model")
    )

    if (Sys.getenv("PRODUCTION_MODE") == "TRUE") {
      # modal open on app launch to warn people
      observe({
        showModal(modalDialog(size = "xl", warning_message, easyClose = FALSE, modalButton("Accept"), footer = NULL))
      })
    }

    # load eucast and update bacteria list
    # [Eucast] ______________________________________________________
    eucast <- update_eucast()
    updateSelectInput(session, "bacteria_select", choices = c("Probabilist" = "probabilist", eucast[[2]]$bacteria))


    # Retrieve Bacteria and antibiotics get the MIC distribution + the ECOFF if available
    # Observe both bacteria and antibiotic selection changes
    observeEvent(list(input$bacteria_select, input$beta_lactamin), {
      if (input$bacteria_select != "probabilist" && !is.null(input$beta_lactamin) && input$beta_lactamin != "") {
        # Store the result in the mic_distribution reactive value
        mic_information(mic_distribution(input$beta_lactamin, input$bacteria_select, eucast))

        # Update the reactive values
        mic_specie(c(as.numeric(names(mic_information()[["mic_distribution"]]))))
        ecoff(as.numeric(mic_information()$ecoff))
        ecoff_ci(mic_information()$ecoff_ci)

        # Debugging in dev mode
        golem::cat_dev("[Module : ptPred] [Line 155] The output of the mic_information object is : \n", "\n")
        golem::print_dev(mic_information())
        golem::cat_dev("[Module : ptPred] [Line 158] The output of the mic_specie object is : \n", "\n")
        golem::print_dev(mic_specie())
        golem::cat_dev("[Module : ptPred] [Line 159] The output of the ecoff object is : \n", "\n")
        golem::print_dev(ecoff())
        golem::cat_dev("[Module : ptPred] [Line 160] The output of the ecoff_ci object is : \n", "\n")
        golem::print_dev(ecoff_ci())
      }
    })


    # [PTA Calculation] ______________________________________________________
    # PTA computing and plotting code
    observeEvent(input$compute_pta, {
      if (is.null(mic_information()) && input$bacteria_select != "probabilist") {
        showNotification("No MIC distribution available for this bacteria", duration = 10, type = "error", closeButton = TRUE)
        return() # Exit the function early
      }

      golem::cat_dev("[Module : ptPred] [Creatinine Unit] The creatinine unit is : ", input$creatinine_unit, "\n", "\n")

      if (!validator$is_valid()) {
        showNotification("Please fix the error displayed before continuing", duration = 10, type = "error", closeButton = TRUE)
        return()
      }

      # general info
      biological <- calc_biological(
        weight = input$weight,
        height = input$height,
        sex = input$sex,
        age = input$age,
        creatinine = input$creatinine,
        urine_creat = input$urine_creatinine,
        urine_output = input$urine_output,
        weight_unit = "kg",
        creat_unit = input$creatinine_unit
      )

      # calculate model parameters (cl and eta_cl) based on selected drug
      model_param <- get_model_parameters(
        model = input$model_selected,
        biological = biological,
        drug = input$beta_lactamin
      )

      golem::cat_dev("[Module : ptPred] [Line 205] The output of the model_param object is : \n", "\n")
      golem::print_dev(model_param)

      # calculate all concentration
      concentration_df <- sim_concentration(
        dose = input$drug_dose * 1000, # convert from g to mg
        tvcl = model_param$cl,
        eta_cl = model_param$eta_cl,
        quantile = input$confidence_level,
        mic = if (input$bacteria_select == "probabilist") NA else mic_specie(),
        dose_increment = model_param$dose_increment * 1000, # convert from g to mg
        toxicity_threshold = ifelse(is.na(drug_threshold(input$beta_lactamin)), 0, drug_threshold(input$beta_lactamin))
      )

      # Debugging in dev mode
      golem::cat_dev("[Module : ptPred] Toxicity level for", input$beta_lactamin, "is", drug_threshold(input$beta_lactamin), " mg/L", "\n", "\n")
      golem::cat_dev("[Module : ptPred] [Line 215] The output of the concentration_df object is : \n", "\n")
      golem::print_dev(concentration_df)

      # [PTA Plot] ___________________________________________________________
      plot <- plot.pta(concentration_df, ecoff = if (input$bacteria_select == "probabilist") NA else ecoff())
      output$pta_output <- renderPlotly({ plotly::ggplotly(plot$pta_multiple_doses) }) # plot pta with css/mic
      output$pta_output_probability <- renderPlotly({ plotly::ggplotly(plot$pta_ci_plot) }) # plot pta with css/mic probability quantile based on user selection

      # [Footer] ___________________________________________________________
      # [Display the dose line] with corresponding colors
      output$footer_pta <- renderUI({
        all_dose <- c(-2, -1, 0, 1, 2) * model_param$dose_increment + input$drug_dose

        # Use the specified colors
        dose_colors <- c("#20846b", "#1f8269", "#2db391", "#32c5a0", "#2fe3b6")

        # Create HTML for the colored dose line
        dose_line <- tags$div(
          style = "display: flex; align-items: center; justify-content: center; margin-top: 10px;",
          lapply(seq_along(all_dose), function(i) {
            if (all_dose[i] > 0) {
              # Use color from our specific palette (cycling if more doses than colors)
              color_idx <- ((i - 1) %% length(dose_colors)) + 1
              tags$div(
                style = paste0(
                  "margin: 0 5px; padding: 3px 8px; background-color: ",
                  dose_colors[color_idx],
                  "; border-radius: 3px; color: white; font-weight: bold;"
                ),
                paste0(all_dose[i], " g")
              )
            }
          })
        )
        return(dose_line)
      })

      # [Display the ECOFF] with confidence interval
      output$footer_pta_probability <- renderUI({
        if (input$bacteria_select != "probabilist") {
          tags$div(
            tags$b("ECOFF :"), ecoff(), " mg/L", tags$span(style = "margin: 0 60px;"), # Add space between elements
            tags$b("ECOFF Confidence Interval: "), ecoff_ci(), " mg/L"
          )
        }
      })

    })
  })
}

## To be copied in the UI
# mod_ptaPred_ui("ptaPred_1")

## To be copied in the server
# mod_ptaPred_server("ptaPred_1")
