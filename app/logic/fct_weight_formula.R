box::use(
  dplyr[case_when]
)

#' Calculate a derived body weight.
#'
#' @param weight Patient weight in kilograms or pounds.
#' @param height Patient height in centimeters.
#' @param sex Patient sex used by the IBW and LBW equations.
#' @param weight_unit Unit for `weight`, either `"kg"` or `"lbs"`.
#' @param formula Derived weight formula to use: `"IBW"`, `"AJBW"`, or `"LBW"`.
#'
#' @return A rounded numeric weight estimate in kilograms.
#' @export
weight_formula <- function(
  weight,
  height,
  sex,
  weight_unit = c("kg", "lbs"),
  formula = c("IBW", "AJBW", "LBW")
) {
  weight <- ifelse(weight_unit == "lbs", weight / 2.20462, weight)
  bmi <- weight / (height / 100)^2
  height_inch <- (height - 152.4) / 2.54
  ibw <- ifelse(sex == "Female", 45.5, 50) + 2.3 * ifelse(height_inch > 0, height_inch, 0)
  ajbw <- ibw + 0.4 * (weight - ibw)
  ffm <- 9270 * weight / (ifelse(sex == "Female", 8780 + 244 * bmi, 6680 + 216 * bmi))

  mod_weight <- case_when(
    formula == "IBW" ~ ibw,
    formula == "AJBW" ~ ajbw,
    formula == "LBW" ~ ffm
  )

  round(mod_weight, digits = 1)
}