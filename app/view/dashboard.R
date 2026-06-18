box::use(
  bs4Dash[dashboardBody, dashboardHeader, dashboardPage, dashboardSidebar, menuItem, sidebarMenu, tabItem, tabItems],
  shiny[actionLink, icon, img, moduleServer, NS, span, tags],
)

box::use(
  app/view/model_information_tab,
  app/view/pta_tab,
  app/view/settings_tab
)

issue_url <- "https://github.com/PharmacoGHN/icuPtaPred/issues"
support_email <- "romain.garreau@univ-lyon1.fr"

app_version <- function() {
  description_path <- normalizePath(file.path(getwd(), "DESCRIPTION"), mustWork = FALSE)

  if (file.exists(description_path)) {
    return(read.dcf(description_path, fields = "Version")[1, 1])
  }

  "1.1.0.1"
}

sidebar_brand <- function(version) {
  tags$div(
    class = "icu-sidebar-brand",
    img(src = "static/hex-icuPtaPred.png", alt = "ICU PTA Predictor icon", class = "icu-sidebar-brand__logo"),
    tags$div(
      class = "icu-sidebar-brand__copy",
      tags$span("ICU PTA Predictor", class = "icu-sidebar-brand__title"),
      tags$span("Clinical PK support", class = "icu-sidebar-brand__subtitle"),
      tags$span(paste0("v", version), class = "icu-sidebar-brand__version")
    )
  )
}

sidebar_panel <- function() {
  tags$div(
    class = "icu-sidebar-panel",
    tags$span("Clinical workflow", class = "icu-sidebar-panel__eyebrow"),
    tags$div(
      class = "icu-sidebar-chipset",
      tags$span("1 Inputs", class = "icu-sidebar-chip"),
      tags$span("2 Simulate", class = "icu-sidebar-chip"),
      tags$span("3 Review", class = "icu-sidebar-chip")
    ),
    tags$p(
      "Move from patient covariates to PTA and then confirm the model context.",
      class = "icu-sidebar-panel__caption"
    )
  )
}

sidebar_footer <- function() {
  tags$div(
    class = "icu-sidebar-footer",
    tags$p(
      "Feedback or missing model",
      class = "icu-sidebar-footer__label"
    ),
    tags$a(
      href = issue_url,
      target = "_blank",
      class = "icu-sidebar-footer__link",
      icon("github"),
      tags$span("Report an issue")
    ),
    tags$a(
      href = paste0("mailto:", support_email),
      class = "icu-sidebar-footer__link",
      icon("envelope"),
      tags$span("Contact the admin")
    )
  )
}

dashboard_brand <- function() {
  tags$span("ICU PTA Predictor", class = "icu-header-title")
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  dashboardPage(
    title = "",
    dark = NULL,
    help = NULL,
    fullscreen = TRUE,
    header = dashboardHeader(
      title = sidebar_brand(app_version()),
      titleWidth = 320,
      compact = TRUE,
      actionLink(
        "reportIssue",
        label = span(icon("github", class = "fa-lg"), " ", style = "color: black;"),
        onclick = paste0("window.open('", "https://github.com/PharmacoGHN/icuPtaPred/", "', '_blank')")
      )
    ),
    sidebar = dashboardSidebar(
      skin = "dark",
      status = "primary",
      tags$div("Workspace", class = "icu-sidebar-nav-heading"),
      sidebarMenu(
        id = ns("navigation"),
        menuItem("PTA Explorer", tabName = "pta", icon = icon("chart-line")),
        menuItem(
          "Model Library",
          tabName = "model_information",
          icon = icon("book-medical")
        ),
        menuItem("Documentation", tabName = "documentation", icon = icon("circle-info"))
      ),
      tags$div("Clinical workflow", class = "icu-sidebar-nav-heading"),
      sidebar_panel(),
      tags$div(class = "icu-sidebar-spacer"),
      sidebar_footer()
    ),
    body = dashboardBody(
      tags$div(
        class = "icu-page-shell",
        tabItems(
          tabItem(tabName = "pta", pta_tab$ui(ns("pta"))),
          tabItem(
            tabName = "model_information",
            model_information_tab$ui(ns("model_information"))
          ),
          tabItem(tabName = "documentation", settings_tab$ui(ns("documentation")))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    model_information_tab$server("model_information")
    pta_tab$server("pta")
    settings_tab$server("documentation")
  })
}