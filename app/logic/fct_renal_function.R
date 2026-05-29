#' Estimate renal function with the selected equation.
#'
#' @param sex Patient sex.
#' @param age Patient age in years.
#' @param weight Patient weight in kilograms.
#' @param height Patient height in centimeters.
#' @param creat Serum creatinine value.
#' @param ethnicity Optional ethnicity flag used by MDRD and CKD-EPI 2009.
#' @param formula Renal function formula to evaluate.
#' @param creat_unit Unit for serum creatinine, either `"uM/L"` or `"mg/dL"`.
#' @param urine_creat Urine creatinine value used by the UV/P calculation.
#' @param urine_output Daily urine output in milliliters used by the UV/P calculation.
#'
#' @return A rounded numeric renal-function estimate.
#' @export
renal_function <- function(
  sex,
  age,
  weight,
  height,
  creat,
  ethnicity = "Caucasian",
  formula,
  creat_unit = "uM/L",
  urine_creat = 0,
  urine_output = 0
) {
  if (isTRUE(creat_unit == "uM/L")) {
    creat_mgdl <- creat / 88.4
    creat_micromol <- creat
    urine_creat_mmol <- urine_creat
  }

  if (isTRUE(creat_unit == "mg/dL")) {
    creat_mgdl <- creat
    creat_micromol <- creat * 88.4
    urine_creat_mmol <- urine_creat * 8.84
  }

  k <- ifelse(sex == "Male", 0.9, 0.7)
  beta_cg <- ifelse(sex == "Male", 1.23, 1.04)
  alpha_2009 <- ifelse(sex == "Male", -0.411, -0.329)
  alpha_2021 <- ifelse(sex == "Male", -0.302, -0.241)
  ckd_female_corr <- ifelse(sex == "Male", 1, 1.018)
  ckd_ethnic_correction <- ifelse(ethnicity == "African", 1.159, 1)

  mdrd_female_corr <- ifelse(sex == "Male", 1, 0.742)
  mdrd_ethnic_correction <- ifelse(ethnicity == "African", 1.212, 1)

  if (formula == "CG") {
    out <- beta_cg * (140 - age) * (weight / creat_micromol)
  }

  if (formula == "MDRD") {
    out <- 175 * creat_mgdl^-1.154 * ifelse(age == 0, 1, age)^-0.203
    out <- out * mdrd_female_corr * mdrd_ethnic_correction
  }

  if (formula == "CKD_2009") {
    out <- 141 * min(creat_mgdl / k, 1)^alpha_2009 * max(creat_mgdl / k, 1)^-1.209
    out <- out * 0.993^age * ckd_female_corr * ckd_ethnic_correction
  }

  if (formula == "CKD_2021") {
    out <- 142 * min(creat_mgdl / k, 1)^alpha_2021 * max(creat_mgdl / k, 1)^-1.20
    out <- out * 0.993^age * ckd_female_corr
  }

  if (formula == "UVP") {
    out <- urine_creat_mmol * urine_output / creat_micromol / (24 * 60) * 1000
  }

  if (formula == "schwartz") {
    k <- ifelse(age < 12, 0.55, 0.7)
    out <- k * height / creat_mgdl
  }

  if (formula == "EKFC") {
    exponent <- ifelse((creat_mgdl / k) < 1, -0.322, -1.132)
    age_exponent <- ifelse(age > 40, age - 40, 0)
    out <- 107.3 * (creat_mgdl / k)^exponent * 0.990^age_exponent
  }

  if (formula == "none") {
    out <- 1
  }

  round(out, digits = 1)
}
