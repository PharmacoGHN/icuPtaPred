#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      header = dashboardHeader(title = "PTA App"),
      sidebar = dashboardSidebar(
        skin = "dark",
        status = "olive",
        sidebarMenu(
          menuItem("PTA", tabName = "ptaGenerator", icon = icon("chart-line")),
          menuItem("Model Info", tabName = "model_information", icon = icon("info")),
          menuItem("Settings", tabName = "app_settings", icon = shiny::icon("gear"))
        ),
        # Add GitHub issue link at the bottom of sidebar
        rep_br(35),
        div(
          style = "padding: 15px;",
          actionLink(
            "reportIssue",
            label = span(icon("github"), " Report Issue"),
            onclick = paste0("window.open('", "https://github.com/PharmacoGHN/icuPtaPred/issues", "', '_blank')")
          ),
          p(style = "font-size: 0.8em; margin-top: 5px; color: white;", 
            "Click to report bugs or suggest improvements on GitHub.")
        )
      ),
      body = dashboardBody(
        tabItems(
          tabItem(tabName = "ptaGenerator", mod_ptaPred_ui("ptaPred_1")),
          tabItem(tabName = "model_information", mod_model_information_ui("model_information_1"))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "icuPtaPred"
    )
    #tags$link(rel = "stylesheet", type = "text/css", href = "www/utilitary.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
