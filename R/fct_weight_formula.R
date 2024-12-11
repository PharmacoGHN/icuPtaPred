# Description
#
# This function returns the weight calculated with the
# selected formula
# IBW : Devine BJ. Gentamicin therapy. DICP. 1974; 8:650–5.
# Adjusted body weight


#' @title weight_formula
#'
#' @description
#' this function calculated different weight indicator, such as
#' the Ideal Body weigh (IBW) or the adjusted bodyweight (AJBW)
#'
#' @param weight correspond to the total body weigt in kg or lbs
#' @param height correspond to the patient height (only in cm)
#' @param sex correspond to the patient sex
#' @param weight_unit should be either kg or lbs
#' @param formula correspond to the formula used. Can take value in c(IBW, AJBW, LBW)

weight_formula <- function(
  weight,
  height,
  sex,
  weight_unit = c("kg", "lbs"),
  formula = c("IBW", "AJBW", "LBW")
) {

  # set weight in kg
  weight <- ifelse(weight_unit == "lbs", weight * 2.20462, weight)
  bmi <- weight / (height / 100) ^ 2

  # calculate size in feet and inches
  height_inch <- (height - 152.4) / 2.54 # 5 feet in cm = 5 * 30.48 ; 1" = 2.54 cm

  # calculate IBW based on Devine formula
  ibw <- ifelse(sex == "Female", 45.5, 50) + 2.3 * ifelse(height_inch > 0, height_inch, 0)
  # calculate adjusted bodyweight
  ajbw <- ibw + 0.4 * (weight - ibw)

  # calculate the Free Fat Mass (Janmahasatian and green, 2005)
  ffm <- 9270 * weight / (ifelse(sex == "Female", 8780 + 244 * bmi, 6680 + 216 * bmi))

  # select the output
  mod_weight <- dplyr::case_when(
    formula == "IBW" ~ ibw,
    formula == "AJBW" ~ ajbw,
    formula == "LBW" ~ ffm
  )

  return(round(mod_weight, digits = 1))
}