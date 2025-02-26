#' model_information UI Function
#'
#' @description A shiny Module.
#'
#' @param id Module id.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_model_information_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$div(
        class = "title-frame",
        h3("Pharmacokinetic Model Information", class = "title-display")
      ),
      br(),
      fluidRow(
        column(
          width = 3,
          # Initially no choices: these will be updated via the server
          selectInput(ns("drug"), "Select Drug:", choices = labels("drug", "choices", lang)),
          selectInput(ns("model"), "Select Model:", choices = "No model currently available")
        ),
        column(
          width = 9,
          div(
            class = "title-frame-section",
            h3(textOutput(ns("title")), class = "title-display")
          ),
          br(),
          p(tags$div(class = "subtitle-display", "Authors: "), uiOutput(ns("authors"))),
          p(tags$div(class = "subtitle-display", "Abstract: "), uiOutput(ns("abstract"))),
          p(tags$div(class = "subtitle-display", "Clearance Formula: "), uiOutput(ns("clearance_formula"))),
          p(tags$div(class = "subtitle-display", "Model Description: "), textOutput(ns("model_description"))),
          p(tags$div(class = "subtitle-display", "Population Studied: "), textOutput(ns("population_studied")))
        )
      )
    )
  )
}

#' model_information Server Functions
#'
#' @param id Module id.
#'
#' @noRd

mod_model_information_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update the list of models when r$drug changes
    observeEvent(input$drug, {
      golem::cat_dev("DEBUG: input$drug is ", input$drug, "\n")
      if (!is.null(input$drug)) {
        updateSelectInput(session, "model", choices = names(model_information[[input$drug]]))
      }
    })

    # Choose the selected model information using r$drug and the selected model name
    selected_model <- reactive({
      req(input$drug, input$model)
      model_information[[input$drug]][[input$model]]
    })

    # Update the UI with the selected model information
    output$title <- renderText({
      selected_model()$Title
    })
    output$authors <- renderUI({
      authors <- paste0(
        selected_model()$Authors, "  ",
        selected_model()$Journal, ",  ",
        selected_model()$Year, ".       "
      )
      golem::cat_dev(authors, "\n")

      #
      tagList(
        authors, "DOI: ",
        a(
          selected_model()$DOI,
          href = if (!is.null(selected_model()$URL)) selected_model()$URL,
          target = "_blank"
        )
      )
    })
    output$abstract <- renderUI({ HTML(selected_model()$Abstract) })
    output$clearance_formula <- renderUI({
      withMathJax(selected_model()$Clearance_Formula)
    })
    output$model_description <- renderText({
      selected_model()$Model_Description
    })
    output$population_studied <- renderText({
      selected_model()$Population_Studied
    })
  })
}
