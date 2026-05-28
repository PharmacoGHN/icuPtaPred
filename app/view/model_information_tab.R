box::use(
  bs4Dash[box],
  shiny
)

box::use(
  app/logic/helper_model_information[model_information],
  app/logic/utils[labels]
)

doi_link <- function(model) {
  has_link <- !is.null(model$URL) && !is.na(model$URL) && grepl("^https?://", model$URL)
  has_doi <- !is.null(model$DOI) && !is.na(model$DOI) && nzchar(model$DOI)

  if (!has_doi) {
    return(NULL)
  }

  if (!has_link) {
    return(shiny$tags$span(model$DOI))
  }

  shiny$tags$a(model$DOI, href = model$URL, target = "_blank")
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    shiny$fluidRow(
      shiny$column(
        width = 3,
        box(
          width = 12,
          title = shiny$tagList(shiny$icon("filter"), "Filters"),
          status = "primary",
          solidHeader = TRUE,
          class = "icu-card",
          shiny$selectInput(
            ns("drug"),
            "Drug",
            choices = labels("drug", "choices", "fr")
          ),
          shiny$selectInput(ns("model"), "Model", choices = "No model currently available")
        )
      ),
      shiny$column(
        width = 9,
        box(
          width = 12,
          title = shiny$textOutput(ns("title")),
          status = "success",
          solidHeader = TRUE,
          class = "icu-card icu-card--article",
          shiny$div(
            class = "icu-detail-block",
            shiny$tags$h4("Citation"),
            shiny$uiOutput(ns("authors"))
          ),
          shiny$div(
            class = "icu-detail-block",
            shiny$tags$h4("Abstract"),
            shiny$uiOutput(ns("abstract"))
          ),
          shiny$div(
            class = "icu-detail-grid",
            shiny$div(
              class = "icu-detail-block",
              shiny$tags$h4("Clearance formula"),
              shiny$uiOutput(ns("clearance_formula"))
            ),
            shiny$div(
              class = "icu-detail-block",
              shiny$tags$h4("Model description"),
              shiny$textOutput(ns("model_description"))
            ),
            shiny$div(
              class = "icu-detail-block",
              shiny$tags$h4("Population studied"),
              shiny$textOutput(ns("population_studied"))
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$observeEvent(input$drug, {
      if (!is.null(input$drug) && nzchar(input$drug)) {
        shiny$updateSelectInput(
          session,
          "model",
          choices = names(model_information[[input$drug]])
        )
      }
    }, ignoreInit = FALSE)

    selected_model <- shiny$reactive({
      shiny$req(input$drug, input$model)
      model_information[[input$drug]][[input$model]]
    })

    output$title <- shiny$renderText({
      selected_model()$Title
    })

    output$authors <- shiny$renderUI({
      model <- selected_model()
      citation <- paste0(model$Authors, " ", model$Journal, ", ", model$Year, ". ")

      shiny$tagList(citation, "DOI: ", doi_link(model))
    })

    output$abstract <- shiny$renderUI({
      shiny$div(class = "icu-prose", shiny$HTML(selected_model()$Abstract))
    })

    output$clearance_formula <- shiny$renderUI({
      shiny$withMathJax(shiny$HTML(selected_model()$Clearance_Formula))
    })

    output$model_description <- shiny$renderText({
      selected_model()$Model_Description
    })

    output$population_studied <- shiny$renderText({
      selected_model()$Population_Studied
    })
  })
}