#' ptaGenerator 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ptaGenerator <- function() {

}

#' rangeMIC 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
rangeMIC <- function(
  min,
  max,
  step,
  default = TRUE,
  drug = c("cefotaxim", "ceftriaxon", "pipTaz","cefepim")
) {
  if(default) {
    # defaut range depending on drug MIC. Excrat Eucast information so it update automatically.
    rangeMIC <- seq()
  } else {
    rangeMIC <- seq()
  }
}
