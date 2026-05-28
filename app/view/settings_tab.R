box::use(
  bs4Dash,
  shiny
)

#' @export
ui <- function(id) {
  shiny::tagList(
    shiny::div(
      class = "icu-tab-hero",
      shiny::tags$span("Guide & settings", class = "icu-tab-hero__eyebrow"),
      shiny::tags$h2("Clinical context and project links", class = "icu-tab-hero__title"),
      shiny::tags$p(
        "Keep the intended use, the data sources, and the feedback channel visible.",
        class = "icu-tab-hero__copy"
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bs4Dash::box(
          width = 12,
          title = shiny::tagList(shiny::icon("circle-info"), "Intended use"),
          status = "primary",
          solidHeader = TRUE,
          class = "icu-card",
          shiny::tags$ul(
            class = "icu-list",
            shiny::tags$li("This tool supports dosing discussions for continuous-infusion beta-lactams."),
            shiny::tags$li("It does not replace bedside judgement, local ecology, or stewardship review."),
            shiny::tags$li("Always review the model assumptions before applying a recommendation.")
          )
        )
      ),
      shiny::column(
        width = 6,
        bs4Dash::box(
          width = 12,
          title = shiny::tagList(shiny::icon("stethoscope"), "Clinical inputs"),
          status = "warning",
          solidHeader = TRUE,
          class = "icu-card",
          shiny::tags$p(
            class = "icu-copy-block",
            "Renal function, infusion strategy, and organism-specific MIC distributions can all shift exposure. Use measured urinary data when a model depends on UV/P-based clearance."
          ),
          shiny::tags$p(
            class = "icu-copy-block",
            "Probability outputs are only as reliable as the selected population model and the data entered into the app."
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        bs4Dash::box(
          width = 12,
          title = shiny::tagList(shiny::icon("link"), "Project resources"),
          status = "success",
          solidHeader = TRUE,
          class = "icu-card",
          shiny::div(
            class = "icu-settings-grid",
            shiny::tags$a(
              href = "https://github.com/PharmacoGHN/icuPtaPred",
              target = "_blank",
              class = "icu-settings-link",
              shiny::icon("code-branch"),
              shiny::tags$span("Repository")
            ),
            shiny::tags$a(
              href = "https://github.com/PharmacoGHN/icuPtaPred/issues",
              target = "_blank",
              class = "icu-settings-link",
              shiny::icon("github"),
              shiny::tags$span("Issue tracker")
            ),
            shiny::tags$a(
              href = "https://appsilon.github.io/rhino/",
              target = "_blank",
              class = "icu-settings-link",
              shiny::icon("book-open"),
              shiny::tags$span("Rhino docs")
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}