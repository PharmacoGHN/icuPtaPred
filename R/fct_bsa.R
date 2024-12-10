# Description
#
# This script contains functions that calculate the body surface area of patient
# based on selected formula (default is DuBois et Dubois, 1916).
# The main purpose of this is to allow clinician to de normalize the renal function


#' @title bsa

#' @description
#' bsa purpose is to calculate the body surface area
#' to de normalize renal function if necessary
#'
#' @param height correspond to patient height in cm
#' @param weight correspond to the patient weight in kg or in lbs
#' @param capped define if the body surface area should be capped at a maximum of 2m² (False by default)
#' @param formula correspond to the formula used to calculate bsa. At the moment the only formula supported is Dubois
#' 
#' @noRd


bsa <- function(height,
                weight,
                capped = FALSE,
                formula = "dubois") {
  if (!is.logical(capped)) {
    stop(message = "Error : capped should be TRUE or FALSE")
  }

  # Dubois, Arch Intern Med. 1916; 17:863-871
  if (isTRUE(formula == "dubois")) {
    body_surface_area <- 0.007184 * height^0.725 * weight^0.425
  }

  if (isTRUE(capped) && body_surface_area > 2) {
    body_surface_area <- 2
  }

  return(round(body_surface_area, digits = 2))
}