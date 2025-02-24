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
          p(strong("Authors: "), textOutput(ns("authors"))),
          p(strong("Abstract: "), textOutput(ns("abstract"))),
          p(strong("Clearance Formula: "), uiOutput(ns("clearance_formula"))),
          p(strong("Model Description: "), textOutput(ns("model_description"))),
          p(strong("Population Studied: "), textOutput(ns("population_studied")))
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
        selected_model()$Authors, ", ",
        selected_model()$Journal,
        selected_model()$Year, ". ", "DOI: ",
        selected_model()$DOI
      )
      

      golem::cat_dev(authors, "\n")
      #tagList(
        a(authors,
          # paste(
          # selected_model()$Authors, ",",
          # selected_model()$Journal,
          # selected_model()$Year, ".", "DOI: ",
          # selected_model()$DOI
          # ),
          href = "https://journals.asm.org/doi/10.1128/aac.02556-19"#,
          #target = "_blank"
        )
      #)
    })
    output$abstract <- renderText({
      selected_model()$Abstract
    })
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
