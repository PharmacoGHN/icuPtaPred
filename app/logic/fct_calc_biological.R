box::use(
  app/logic/fct_bsa[bsa],
  app/logic/fct_renal_function[renal_function],
  app/logic/fct_weight_formula[weight_formula]
)

#' Derive patient biological covariates used by the PTA models.
#'
#' @param sex Patient sex.
#' @param age Patient age in years.
#' @param weight Patient weight in kilograms or pounds.
#' @param height Patient height in centimeters.
#' @param creatinine Serum creatinine value.
#' @param weight_unit Unit for `weight`, either `"kg"` or `"lbs"`.
#' @param creat_unit Unit for `creatinine`, either `"uM/L"` or `"mg/dL"`.
#' @param urine_creat Urine creatinine value used by the UV/P calculation.
#' @param urine_output Daily urine output in milliliters used by the UV/P calculation.
#'
#' @return A named list of body-size metrics and renal-function estimates.
#' @export
calc_biological <- function(sex, age, weight, height, creatinine, weight_unit, creat_unit, urine_creat, urine_output) {
  weight_kg <- if (identical(weight_unit, "lbs")) {
    weight / 2.20462
  } else {
    weight
  }

  tbw <- weight_kg
  lbw <- weight_formula(weight, height, sex, weight_unit, formula = "LBW")
  ajbw <- weight_formula(weight, height, sex, weight_unit, formula = "AJBW")
  ibw <- weight_formula(weight, height, sex, weight_unit, formula = "IBW")
  bmi <- round(weight_kg / (height / 100)^2, digits = 1)
  bsa_value <- bsa(height, weight_kg)
  cg_tbw <- renal_function(sex, age, tbw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_ajbw <- renal_function(sex, age, ajbw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_ibw <- renal_function(sex, age, ibw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_lbw <- renal_function(sex, age, lbw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  mdrd <- renal_function(sex, age, tbw, height, creatinine, formula = "MDRD", creat_unit = creat_unit)
  ckd_2009 <- renal_function(sex, age, tbw, height, creatinine, formula = "CKD_2009", creat_unit = creat_unit)
  ckd_2021 <- renal_function(sex, age, tbw, height, creatinine, formula = "CKD_2021", creat_unit = creat_unit)
  schwartz <- renal_function(sex, age, tbw, height, creatinine, formula = "schwartz", creat_unit = creat_unit)
  uvp <- renal_function(sex, age, tbw, height, creatinine, formula = "UVP", creat_unit = creat_unit, urine_creat = urine_creat, urine_output = urine_output)
  ekfc <- renal_function(sex, age, tbw, height, creatinine, formula = "EKFC", creat_unit = creat_unit)

  list(
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
    uvp = uvp,
    ekfc = ekfc
  )
}
