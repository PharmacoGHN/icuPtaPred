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
