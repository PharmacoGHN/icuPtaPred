box::use(
  bs4Dash[box],
  shiny
)

documentation_step <- function(number, title, copy) {
  shiny$tags$div(
    class = "icu-doc-step",
    shiny$tags$span(number, class = "icu-doc-step__index"),
    shiny$tags$div(
      class = "icu-doc-step__body",
      shiny$tags$h4(title, class = "icu-doc-step__title"),
      shiny$tags$p(copy, class = "icu-doc-step__copy")
    )
  )
}

documentation_point <- function(title, copy) {
  shiny$tags$div(
    class = "icu-doc-point",
    shiny$tags$h4(title),
    shiny$tags$p(copy)
  )
}

#' @export
ui <- function(id) {
  shiny$tagList(
    shiny$fluidRow(
      shiny$column(
        width = 12,
        shiny$tags$div(
          class = "icu-doc-banner",
          shiny$tags$div(
            class = "icu-doc-banner__lead",
            shiny$tags$span("Documentation", class = "icu-doc-banner__eyebrow"),
            shiny$tags$h2(
              "What ICU PTA Predictor is designed to support",
              class = "icu-doc-banner__title"
            ),
            shiny$tags$p(
              "ICU PTA Predictor helps clinicians explore whether a continuous-infusion beta-lactam regimen is likely to reach pharmacodynamic targets in critically ill patients.",
              class = "icu-doc-banner__copy"
            )
          ),
          shiny$tags$div(
            class = "icu-doc-feature-list",
            shiny$tags$span("Patient covariates", class = "icu-doc-feature"),
            shiny$tags$span("Population PK models", class = "icu-doc-feature"),
            shiny$tags$span("EUCAST MIC data", class = "icu-doc-feature")
          )
        )
      )
    ),
    shiny$fluidRow(
      shiny$column(
        width = 6,
        box(
          width = 12,
          title = shiny$tagList(shiny$icon("bullseye"), "What this application does"),
          status = "primary",
          solidHeader = TRUE,
          class = "icu-card",
          shiny$tags$ul(
            class = "icu-doc-list",
            shiny$tags$li("Simulates expected steady-state exposure for continuous-infusion beta-lactams."),
            shiny$tags$li("Shows how candidate dose steps compare with MIC-based pharmacodynamic targets."),
            shiny$tags$li("Adds CFR when a bacterium is selected and EUCAST distribution data are available.")
          )
        )
      ),
      shiny$column(
        width = 6,
        box(
          width = 12,
          title = shiny$tagList(shiny$icon("sliders-h"), "What you enter"),
          status = "warning",
          solidHeader = TRUE,
          class = "icu-card",
          shiny$tags$ul(
            class = "icu-doc-list",
            shiny$tags$li("Drug, daily dose, and optional organism selection."),
            shiny$tags$li("Age, height, weight, sex, and serum creatinine."),
            shiny$tags$li("Urinary creatinine and urine output when a model depends on measured renal function.")
          )
        )
      )
    ),
    shiny$fluidRow(
      shiny$column(
        width = 6,
        box(
          width = 12,
          title = shiny$tagList(shiny$icon("list-ol"), "Recommended workflow"),
          status = "success",
          solidHeader = TRUE,
          class = "icu-card",
          shiny$tags$div(
            class = "icu-doc-step-list",
            documentation_step("1", "Choose the clinical scenario", "Select the drug, enter the regimen, and decide whether you want a probabilistic view or an organism-specific assessment."),
            documentation_step("2", "Enter the patient profile", "Fill in the patient anthropometric and renal data that the selected population PK model needs."),
            documentation_step("3", "Compute PTA", "Generate the exposure curves, probability interval, and CFR view when organism data are available."),
            documentation_step("4", "Review the model context", "Open the model library to confirm the study population and clearance formula behind the displayed output.")
          )
        )
      ),
      shiny$column(
        width = 6,
        box(
          width = 12,
          title = shiny$tagList(shiny$icon("chart-line"), "How to read the outputs"),
          status = "info",
          solidHeader = TRUE,
          class = "icu-card",
          documentation_point(
            "Dose-response",
            "Shows the expected Css/MIC line for the chosen dose and the nearby dose steps, so you can see how exposure moves when the regimen is increased or reduced."
          ),
          documentation_point(
            "Probability interval",
            "Highlights the spread caused by population variability and helps you judge how robust the exposure target is around the selected dose."
          ),
          documentation_point(
            "CFR",
            "Summarizes how well a regimen is expected to cover the selected organism distribution instead of a single MIC value."
          )
        )
      )
    ),
    shiny$fluidRow(
      shiny$column(
        width = 12,
        box(
          width = 12,
          title = shiny$tagList(shiny$icon("shield-alt"), "Purpose and boundaries"),
          status = "success",
          solidHeader = TRUE,
          class = "icu-card",
          shiny$tags$div(
            class = "icu-doc-note",
            shiny$tags$p(
              "This application is intended for dosing support and discussion. It helps frame expected exposure, but it does not replace bedside judgement, local microbiology, stewardship review, or therapeutic drug monitoring."
            ),
            shiny$tags$p(
              "Use the model library when you want to confirm whether the population, renal descriptor, and clinical setting behind a result are appropriate for the patient in front of you."
            )
          ),
          shiny$tags$div(
            class = "icu-settings-grid icu-doc-links",
            shiny$tags$a(
              href = "https://github.com/PharmacoGHN/icuPtaPred",
              target = "_blank",
              class = "icu-settings-link",
              shiny$icon("code-branch"),
              shiny$tags$span("Project repository")
            ),
            shiny$tags$a(
              href = "https://github.com/PharmacoGHN/icuPtaPred/issues",
              target = "_blank",
              class = "icu-settings-link",
              shiny$icon("github"),
              shiny$tags$span("Report an issue")
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
  })
}