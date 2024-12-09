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
#' @param continuous if TRUE, the MIC range will be continuous
#' @param drug not supported at the moment
#' @param bacteria not supported at the moment, will be used to get ECOFF with automated extraction from EUCAST
#'
#' @return rangeMIC
#'
#' @noRd
rangeMIC <- function(
  min = 0.0625,
  max = 64,
  step = 2,
  continuous = FALSE,
  drug = c("cefotaxim", "ceftriaxon", "pipTaz","cefepim"),
  bacteria
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
