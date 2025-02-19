#' model_information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_model_information_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Pharmacokinetic Model Information"),
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("model"), "Select Model:", choices = names(model_information))
        ),
        mainPanel(
          h3(textOutput(ns("title"))),
          p(strong("Authors: "), textOutput(ns("authors"))),
          p(strong("Year: "), textOutput(ns("year"))),
          p(strong("Journal: "), textOutput(ns("journal"))),
          p(strong("DOI: "), textOutput(ns("doi"))),
          p(strong("Abstract: "), textOutput(ns("abstract"))),
          p(strong("Clearance Formula: "), textOutput(ns("clearance_formula"))),
          p(strong("Model Description: "), textOutput(ns("model_description"))),
          p(strong("Population Studied: "), textOutput(ns("population_studied")))
        )
      )
    )
  )
}

#' model_information Server Functions
#'
#' @noRd
mod_model_information_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # allow model selection to desplay information
    selected_model <- reactive({ model_information[[input$model]] })

    output$title <- renderText({ selected_model()$Title })
    output$authors <- renderText({ selected_model()$Authors })
    output$year <- renderText({ selected_model()$Year })
    output$journal <- renderText({ selected_model()$Journal })
    output$doi <- renderText({ selected_model()$DOI })
    output$abstract <- renderText({ selected_model()$Abstract })
    output$clearance_formula <- renderText({ selected_model()$Clearance_Formula })
    output$model_description <- renderText({ selected_model()$Model_Description })
    output$population_studied <- renderText({ selected_model()$Population_Studied })

  })
}

## To be used in UI:
# mod_model_information_ui("model_information_1")

## To be used in server:
# mod_model_information_server("model_information_1")
