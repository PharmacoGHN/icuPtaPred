# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(
  shiny.port = httpuv::randomPort(),
  shiny.testmode = TRUE,
  shiny.fullstacktrace = TRUE,
  shiny.autoreload = TRUE,
  shiny.launch.browser = TRUE
)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()

# Document and reload your package
golem::document_and_reload()

# Run the application
icuPtaPred::run_app()
