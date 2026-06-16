box::use(
  bs4Dash[box, tabsetPanel],
  shiny[
    actionLink, column, fluidRow, icon, moduleServer, navlistPanel, NS, span, tags, tagList, tabPanel, tabsetPanel, uiOutput
  ],
)

box::use(
  app/logic/utils[orcid_icon]
)

issue_url <- "https://github.com/PharmacoGHN/icuPtaPred/issues"
support_email <- "romain.garreau@univ-lyon1.fr"

documentation_step <- function(number, title, copy) {
  tags$div(
    class = "icu-doc-step",
    tags$span(number, class = "icu-doc-step__index"),
    tags$div(
      class = "icu-doc-step__body",
      tags$h4(title, class = "icu-doc-step__title"),
      tags$p(copy, class = "icu-doc-step__copy")
    )
  )
}

documentation_point <- function(title, copy) {
  tags$div(
    class = "icu-doc-point",
    tags$h4(title),
    tags$p(copy)
  )
}

software_information_section <- function() {
  tags$div(
    class = "general-info-section",
    tags$h4("Software Information", class = "general-info-section__title"),
    tags$hr(),
    tags$h5("Authors:", class = "general-info-section__heading"),
    tags$hr(),
    tags$p(
      class = "general-info-section__authors",
      orcid_icon("Lisa Leyenberger"), ", ",
      orcid_icon("Romain Garreau", "0000-0002-6605-4808"), ", ",
      orcid_icon("Arnaud Friggeri", "0000-0003-4687-2173"), ", ",
      orcid_icon("Sylvain Goutelle", "0000-0002-1853-2932")
    ),
    tags$br(),
    tags$h5("Publications:", class = "general-info-section__heading"),
    tags$hr(),
    tags$p("Future publication title placeholder", class = "general-info-section__copy"),
    tags$a(
      "Future DOI/link placeholder",
      href = "#",
      class = "general-info-section__link"
    ),
    tags$br(),
    tags$br(),
    tags$h5("General information:", class = "general-info-section__heading"),
    tags$hr(),
    tags$div(
      class = "general-info-section__meta",
      tags$span(tags$strong("Version: "), "1.0.0"),
      tags$br(),
      tags$span(tags$strong("Date: "), "2025-06-28"),
      tags$br(),
      tags$span(tags$strong("License:"), " AGPL-3")
    )
  )
}

#' @export
ui <- function(id) {
  tagList(
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "icu-doc-banner",
          tags$div(
            class = "icu-doc-banner__lead",
            tags$span("Documentation", class = "icu-doc-banner__eyebrow"),
            tags$h2("What ICU PTA Predictor is designed to support", class = "icu-doc-banner__title"),
            tags$p(
              "ICU PTA Predictor helps clinicians explore whether a continuous-infusion beta-lactam regimen is likely to reach pharmacodynamic targets in critically ill patients.",
              class = "icu-doc-banner__copy"
            )
          ),
          tags$div(
            class = "icu-doc-feature-list",
            tags$span("Patient covariates", class = "icu-doc-feature"),
            tags$span("Population PK models", class = "icu-doc-feature"),
            tags$span("EUCAST MIC data", class = "icu-doc-feature")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        bs4Dash::tabsetPanel(
          vertical = TRUE,
          type = "pills",
          tabPanel(
            "Overview",
            tagList(
              tags$div(
                class = "icu-doc-note",
                tags$p("This application is intended for dosing support and discussion. It helps frame expected exposure, but it does not replace bedside judgement, local microbiology, stewardship review, or therapeutic drug monitoring."),
                tags$p("Use the model library when you want to confirm whether the population, renal descriptor, and clinical setting behind a result are appropriate for the patient in front of you."),
                tags$p("This tool is only intended to help structure thinking around a dose that has already been selected for discussion. It must not be used to choose or prescribe dosing, and the maintainers decline responsibility for clinical decisions made from this tool alone.")
              )
            ),
            documentation_point(
              "features",
              paste0("Simulates expected steady-state exposure for continuous-infusion beta-lactams. \n",
              "Shows how candidate dose steps compare with MIC-based pharmacodynamic targets. \n",
              "Adds CFR when a bacterium is selected and EUCAST distribution data are available.")
            ),
            software_information_section()
          ),
          tabPanel(
            "Workflow",
            tags$div(
              class = "icu-doc-step-list",
              documentation_step("1", "Choose the clinical scenario", "Select the drug, enter the regimen, and decide whether you want a probabilistic view or an organism-specific assessment."),
              documentation_step("2", "Enter the patient profile", "Fill in the patient anthropometric and renal data that the selected population PK model needs."),
              tagList(
                documentation_point("", "Drug, daily dose, and optional organism selection."),
                documentation_point("", "Age, height, weight, sex, and serum creatinine."),
                documentation_point("", "Urinary creatinine and urine output when a model depends on measured renal function.")
              ),
              documentation_step("3", "Compute PTA", "Generate the exposure curves, probability interval, and CFR view when organism data are available."),
              documentation_step("4", "Review the model context", "Open the model library to confirm the study population and clearance formula behind the displayed output."),
              tagList(
                documentation_point("Dose-response", "Shows the expected Css/MIC line for the chosen dose and the nearby dose steps, so you can see how exposure moves when the regimen is increased or reduced."),
                documentation_point("Probability interval", "Highlights the spread caused by population variability and helps you judge how robust the exposure target is around the selected dose."),
                documentation_point("CFR", "Summarizes how well a regimen is expected to cover the selected organism distribution instead of a single MIC value.")
              )
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}