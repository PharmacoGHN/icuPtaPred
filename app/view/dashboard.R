box::use(
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, tabItems, tabItem],
  shiny[NS, tags, icon, img, fluidRow, column, selectInput, textOutput, uiOutput, moduleServer],
)

box::use(
  app/view/model_information_tab,
  app/view/pta_tab,
  app/view/settings_tab
)

issue_url <- "https://github.com/PharmacoGHN/icuPtaPred/issues"

sidebar_footer <- function() {
  tags$div(
    class = "icu-sidebar-footer",
    tags$p(
      "Need a correction or a missing model?",
      class = "icu-sidebar-footer__label"
    ),
    tags$a(
      href = issue_url,
      target = "_blank",
      class = "icu-sidebar-footer__link",
      icon("github"),
      tags$span("Report an issue")
    )
  )
}

dashboard_brand <- function() {
  tags$div(
    class = "icu-brand",
    img(src = "hex-icuPtaPred.png", alt = "ICU PTA Predictor", class = "icu-brand__logo"),
    tags$div(
      class = "icu-brand__copy",
      tags$span("ICU PTA Predictor", class = "icu-brand__title"),
      tags$span("Rhino clinical dashboard", class = "icu-brand__subtitle")
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  dashboardPage(
    title = "ICU PTA Predictor",
    header = dashboardHeader(
      title = dashboard_brand(),
      titleWidth = 320
    ),
    sidebar = dashboardSidebar(
      skin = "dark",
      status = "primary",
      tags$div(
        class = "icu-sidebar-intro",
        tags$span("Precision beta-lactam support", class = "icu-sidebar-intro__eyebrow"),
        tags$p(
          "Explore PTA, compare PK models, and keep clinical context visible.",
          class = "icu-sidebar-intro__body"
        )
      ),
      sidebarMenu(
        id = ns("navigation"),
        menuItem("PTA Explorer", tabName = "pta", icon = icon("chart-line")),
        menuItem(
          "Model Library",
          tabName = "model_information",
          icon = icon("book-medical")
        ),
        menuItem("Guide & Settings", tabName = "settings", icon = icon("sliders"))
      ),
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
          tabItem(tabName = "settings", settings_tab$ui(ns("settings")))
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
    settings_tab$server("settings")
  })
}