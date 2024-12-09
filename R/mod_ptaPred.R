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
            title = "dose",
            numericInput("drug_dose", "Dose Antibiotique (mg)", value = 1000, step = 1, min = 0, max = 32000)
          )
        ),
        column(width = 8,
          box(
            width = 12,
            status = "olive",
            solidHeader = TRUE, 
            title = "PTA output"
          )
        )
      ), 
      fluidRow(
        cat("Disclamer\n
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
