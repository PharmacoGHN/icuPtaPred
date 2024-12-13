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
            selectInput(ns("beta_lactamin"), "Beta lactamin", choices = c("Amox", "Cefepim"), selected = character(0)),
            selectInput(ns("administration_route"), "Voie Administration", choices = c("Per Os" = "PO", "Intraveneux" = "IV", "Sous Cutane" = "SC", "Intramusculaire" = "IM"), selected = "IV"),
            selectInput(ns("administration_interval"), "Interval Administration", choices = c("q48h", "q24h", "q12h", "q8h", "q6h", "q4h", "continue"), selected = "continue"),
            numericInput(ns("drug_dose"), "Dose Antibiotique (mg)", value = 1000, step = 1, min = 0, max = 32000),
            br(),
            selectInput(ns("bacteria_select"), "Selectionner Bacterie", choices = c("Traitement Probabiliste" = "probabilist", "other"), selected = "probabilist")
          ),
          box(
            width = 12,
            status = "lightblue",
            solidHeader = TRUE,
            title = "Information Patient",
            numericInput(ns("age"), label = "Age (ans)", value = 0, min = 0, max = 1000, step = 1),
            numericInput(ns("height"), label = "Taille (cm)", value = 0, min = 0, max = 1000, step = 1),
            numericInput(ns("weight"), label = "Poids (kg)", value = 0, min = 0, max = 1000, step = 1),
            numericInput(ns("creatinine"), label = "Creatinine (umol/L)", value = 0, min = 0, max = 1000, step = 1)
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
              title = "PTA output"
            ),
            box(
              width = 12,
              solidHeader = FALSE,
              title = "PTA Probability output"
            ),
            footer = fluidRow(
              actionButton(ns("compute_pta"), "Generer les PTA", style = "background-color: #3d9970; color: white;")
            )
          )
        ),
        column(
          width = 3,
          tagList(
            div(
              class = "information-panel pull-right",
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
      # get patient data and
      patient_data <- create_patient_data(
        sex = input$sex,
        age = input$age,
        weight = input$weight,
        height = input$height,
        creatinine = input$creatinine,
        creat_unit = "uM/L",
        weight_unit = "kg"
      )

      # model information
      model_parameters <- get_model_parameters(
        patient_data,
        drug = input$beta_lactamin,
        model = 1,
        model_bank = model_bank
      )

      # calculate css
      cl_distribution <- stats::rnorm(10000, mean = model_parameters$tvcl, sd = model_parameters$eta_cl)
      css_distribution <- cl_distribution
      css_mic_distribution <- css_distribution / range
    })
  })
}

## To be copied in the UI
# mod_ptaPred_ui("ptaPred_1")

## To be copied in the server
# mod_ptaPred_server("ptaPred_1")
