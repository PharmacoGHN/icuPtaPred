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
#' @description Internal function that generate the MIC range to be plotted against
#' 
#' @param min min MIC value, by default = 0.0625 mg/L
#' @param max max MIC Value, by default = 64 mg/L
#' @param step multiplicative coefficient to get the next MIC value (from min to max)
#' @param default
#' @param continuous
#' @param drug
#' 
#' @return rangeMIC
#'
#' @noRd
rangeMIC <- function(
  min = 0.0625,
  max = 64,
  step = 2,
  default = TRUE,
  continuous = FALSE,
  drug = c("cefotaxim", "ceftriaxon", "pipTaz","cefepim")
) {

  if (continuous) {
    rangeMIC <- seq(min, max, by = 0.1)
  } else {
    n <- log(max / min) / log(step) # number of step to power n to go from min to max.
    rangeMIC <- min
    for (i in seq_len(n)) rangeMIC <- c(rangeMIC, min * step ^ i)
  }

  return(rangeMIC)
}
