#' Calculate body surface area.
#'
#' @param height Patient height in centimeters.
#' @param weight Patient weight in kilograms.
#' @param capped Whether to cap the calculated body surface area at 2 m^2.
#' @param formula Body-surface-area formula to use. Currently only `"dubois"` is supported.
#'
#' @return A numeric body surface area in square meters, rounded to 2 decimals.
#' @export
bsa <- function(height, weight, capped = FALSE, formula = "dubois") {
  if (!is.logical(capped)) {
    stop("Error: capped should be TRUE or FALSE")
  }

  if (isTRUE(formula == "dubois")) {
    body_surface_area <- 0.007184 * height^0.725 * weight^0.425
  }

  if (isTRUE(capped) && body_surface_area > 2) {
    body_surface_area <- 2
  }

  round(body_surface_area, digits = 2)
}