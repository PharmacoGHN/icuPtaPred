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
            #numericInput(ns("cystatin_c"), label = labels("cystatin_c", "label", lang), value = 0, min = 0, max = 1500, step = 1),
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
              plotlyOutput(ns("pta_output"))
            ),
            box(
              width = 12,
              solidHeader = TRUE,
              status = "olive",
              collapsible = FALSE,
              title = "PTA Probability output",
              plotlyOutput(ns("pta_output_probability"))
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
            selectInput(ns("beta_lactamin"), label = labels("drug", "label", lang), choices = labels("drug", "choices", lang), selected = character(0), width = "auto"),
            selectInput(ns("model_selected"), label = "Select Model:", choices = character(0), width = "auto"),
            uiOutput(ns("model_choice")),
            numericInput(ns("drug_dose"), label = labels("dose_input", "label", lang), value = 0, step = 0.125, min = 0, max = 32, width = "auto"),
            br(),
            selectInput(ns("bacteria_select"), "Selectionner Bacterie", choices = c("Traitement Probabiliste" = "probabilist", "other"), selected = "probabilist", width = "auto")
          ),
          br(),
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
    
    # [Validator] _______________________________________________
    validator <- InputValidator$new()
    validator$add_rule("drug_dose", function(value) { if (value == 0) "Dose must be greater than 0" })
    validator$add_rule("height", function(value) { if (value < 10) "Height must be in cm" })
    validator$add_rule("height", function(value) { if (value > 250) "Height must be less than 250 cm" })
    validator$add_rule("weight", function(value) { if (value < 1) "Weight must be in kg" })
    validator$add_rule("weight", function(value) { if (value > 500) "Weight must be less than 500 kg" })
    validator$add_rule("age", function(value) { if (value <= 0) "Age must be greater than 0" })
    validator$add_rule("age", function(value) { if (value > 120) "Age must be less than 120" })
    validator$add_rule("model_selected", function(value) { if (value == "Barreto_2023") "Not currently supported" })
    validator$add_rule("model_selected", function(value) { if (value == "Gijsen_2021") "Not currently supported" })
    validator$add_rule("model_selected", function(value) { if (value == "Minichmayr_2018") "Not currently supported" })
    validator$add_rule("model_selected", function(value) { if (value == "Ehrmann_2019") "Not currently supported" })
    validator$add_rule("model_selected", function(value) { if (value == "Huang_2025") "Not currently supported" })
    validator$add_rule("model_selected", function(value) { if (value == "Lan_2022") "Not currently supported" })

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

    # [PTA Calculation] ______________________________________________________
    # PTA computing and plotting code
    observeEvent(input$compute_pta, {

      golem::cat_dev("[Module : ptPred] [Creatinine Unit] The creatinine unit is : ", input$creatinine_unit, "\n")

      if (!validator$is_valid()) showNotification("Please fix the error displayed before continuing", duration = 10, type = "error", closeButton = TRUE)
      if (validator$is_valid()) {
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

        # TODO fix all model and get all model working
        # calculate model parameters (cl and eta_cl) based on selected drug
        model_param <- get_model_parameters(
          model = input$model_selected,
          biological = biological,
          drug = input$beta_lactamin
        )

        golem::cat_dev("[Module : ptPred] [Line 154] The output of the model_param object is : \n")
        golem::print_dev(model_param)

        # calculate all concentration
        concentration_df <- sim_concentration(
          dose = input$drug_dose * 1000, # convert from g to mg
          tvcl = model_param$cl,
          eta_cl = model_param$eta_cl,
          quantile = input$confidence_level,
          dose_increment = model_param$dose_increment * 1000, # convert from g to mg
          toxicity_threshold = ifelse(is.na(drug_threshold(input$beta_lactamin)), 0, drug_threshold(input$beta_lactamin))
        )

        # Debugging in dev mode
        golem::cat_dev("[Module : ptPred] Toxicity level for", input$beta_lactamin, "is", drug_threshold(input$beta_lactamin), " mg/L", "\n")
        golem::cat_dev("[Module : ptPred] [Line 154] The output of the concentration_df object is : \n")
        golem::print_dev(concentration_df)

        # [PTA Plot] ___________________________________________________________
        plot <- plot.pta(concentration_df)
        output$pta_output <- renderPlotly({ plotly::ggplotly(plot$pta_multiple_doses) }) # plot pta with css/mic
        output$pta_output_probability <- renderPlotly({ plotly::ggplotly(plot$pta_ci_plot) }) # plot pta with css/mic probability quantile based on user selection
      }
    })
  })
}

## To be copied in the UI
# mod_ptaPred_ui("ptaPred_1")

## To be copied in the server
# mod_ptaPred_server("ptaPred_1")
