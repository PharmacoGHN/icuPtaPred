#' calc_biological
#'
#' @description A fct function
#'
#' @param sex .
#' @param age .
#' @param weight .
#' @param height .
#' @param creatinine .
#' @param weight_unit .
#' @param creat_unit .
#' @param urine_creat .
#' @param urine_output .
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

calc_biological <- function(sex, age, weight, height, creatinine, weight_unit, creat_unit, urine_creat, urine_output) {
  tbw <- weight
  lbw <- weight_formula(weight, height, sex, weight_unit, formula = "LBW")
  ajbw <- weight_formula(weight, height, sex, weight_unit, formula = "AJBW")
  ibw <- weight_formula(weight, height, sex, weight_unit, formula = "IBW")
  bmi <- round(weight / (height / 100)^2, digits = 1)
  bsa_value <- bsa(height, weight)
  cg_tbw <- renal_function(sex, age, tbw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_ajbw <- renal_function(sex, age, ajbw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_ibw <- renal_function(sex, age, ibw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_lbw <- renal_function(sex, age, lbw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  mdrd <- renal_function(sex, age, tbw, height, creatinine, formula = "MDRD", creat_unit = creat_unit)
  ckd_2009 <- renal_function(sex, age, tbw, height, creatinine, formula = "CKD_2009", creat_unit = creat_unit)
  ckd_2021 <- renal_function(sex, age, tbw, height, creatinine, formula = "CKD_2021", creat_unit = creat_unit)
  schwartz <- renal_function(sex, age, tbw, height, creatinine, formula = "schwartz", creat_unit = creat_unit)
  uvp <- renal_function(sex, age, tbw, height, creatinine, formula = "UVP", creat_unit = creat_unit, urine_creat = urine_creat, urine_output = urine_output)

  output <- list(
    tbw = tbw,
    lbw = lbw,
    ajbw = ajbw,
    ibw = ibw,
    bmi = bmi,
    bsa = bsa_value,
    cg_tbw = cg_tbw,
    cg_ajbw = cg_ajbw,
    cg_ibw = cg_ibw,
    cg_lbw = cg_lbw,
    mdrd = mdrd,
    ckd_2009 = ckd_2009,
    ckd_2021 = ckd_2021,
    schwartz = schwartz,
    uvp = uvp
  )

  return(output)
}
