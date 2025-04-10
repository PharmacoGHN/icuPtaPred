#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # create the warning message to display on launch
    # [Warning - Disclamer] ______________________________________
    warning_message <- div(
      class = "disclamer-panel pull-right",
      p("Disclamer", style = "font-weight: bold; font-size: 16px; text-align: center;"),
      p("1. Aide a la decision"),
      p("2. ne prend pas en compte l ecologie locale"),
      p("3. regarder le modele sous jacent (defaut ICU) mais specificite des modeles decrites dans longlet model")
    )

    if (golem::app_prod()) {
      # modal open on app launch to warn people
      observe({
        showModal(modalDialog(size = "xl", warning_message, easyClose = FALSE, modalButton("Accept"), footer = NULL))
      })
    }



  # Your application server logic
  mod_ptaPred_server("ptaPred_1")
  mod_model_information_server("model_information_1")
}
