#' ptaPred UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ptaPred_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(width = 4,
          box(
            width = 12,
            status = "olive",
            solidHeader = TRUE,
            title = "Information sur le Traitement",
            selectInput("beta_lactamin", "Beta lactamin",  choices = c("Amox", "Cefepim"), selected = character(0)),
            selectInput("administration_route", "Voie Administration",  choices = c("Per Os" = "PO", "Intraveneux" = "IV", "Sous Cutane" = "SC","Intramusculaire" = "IM"), selected = "IV"),
            selectInput("administration_interval", "Interval Administration",  choices = c("q48h", "q24h", "q12h", "q8h", "q6h", "q4h", "continue"), selected = "continue"),
            numericInput("drug_dose", "Dose Antibiotique (mg)", value = 1000, step = 1, min = 0, max = 32000),
            br(),
            selectInput("bacteria_select", "Selectionner Bacterie", choices = c("Traitement Probabiliste" = "probabilist", "other"), selected = "probabilist")
          ),
          box(
            width = 12,
            status = "olive",
            solidHeader = TRUE,
            title = "Information Patient",
            numericInput(inputId = "age", label = "Age (ans)", value = 0, min = 0, max = 1000, step = 1),
            numericInput(inputId = "height", label = "Taille (cm)", value = 0, min = 0, max = 1000, step = 1),
            numericInput(inputId = "weight", label = "Poids (kg)", value = 0, min = 0, max = 1000, step = 1),
            numericInput(inputId = "creatinine", label = "Creatinine (umol/L)", value = 0, min = 0, max = 1000, step = 1)
            # choice ethnicity
            # add all patient info to be computed in pop pk model (no bayesian?)
          )
        ),
        column(width = 1),
        column(width = 7,
          box(
            width = 12,
            status = "olive",
            solidHeader = TRUE,
            title = "PTA output",
            actionButton(ns("compute_pta"), "Generer les PTA", style = "background-color: #3d9970; color: white;")
          )
          
        )
      ),
      fluidRow(
        renderText("Disclamer\n
          1. Aide a la decision\n
          2. ne prend pas en compte l ecologie locale\n
          3. regarder le modele sous jacent (defaut ICU) mais specificite des modeles decrites dans longlet model"
        )
      )
    )
  )
}

#' ptaPred Server Functions
#'
#' @noRd
mod_ptaPred_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_ptaPred_ui("ptaPred_1")

## To be copied in the server
# mod_ptaPred_server("ptaPred_1")
