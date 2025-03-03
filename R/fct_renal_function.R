# Description
#
# This function calculated directly return the renal clearance based on the
# selected formula. It also allow user to chose if they want the creatinine
# express with the EU or US unit. Of note, renal function with cystatin-C
# are currently not supported.



#' @title renal_function

#' @param sex patient sex can be either 'Male' or 'Female'
#' @param age age of the patient
#' @param weight weight of the patient (can either be in kg or lbs)
#' @param height patient height.
#' @param creat serum creatinine can be given in mg/dL or uM/L
#' @param ethnicity can be African or other, is set to caucasian by default
#' @param formula the name of the formula needed to calculate the renal function. Can take any value in c('CG', 'mdrd', 'ckd_2009', 'ckd_2021', 'UVP', 'none').
#'  These stand for Cockcroft and Gault, mdrd 4 variables, ckd-EPI 2009 and 2021 formula and calculated uv/p, respectively
#' @param creat_unit give the unit in which the creatinine was entered. Must be "uM/L" or "mg/dL". The unit is set to "uM/L" by default.
#' @param urine_creat the urine creatinine level. The unit is given by the parameter creat_unit
#' @param urine_output correspond to the daily urine output in mL. Note that this is the only unit that is supported
#'
#' @noRd

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
  # define creat unit : ______________________________________________
  if (isTRUE(creat_unit == "uM/L")) {
    creat_mgdl <- creat / 88.4
    creat_micromol <- creat
    urine_creat_mmol <- urine_creat
  }

  if (isTRUE(creat_unit == "mg/dL")) {
    creat_mgdl <- creat
    creat_micromol <- creat * 88.4
    urine_creat_mmol <- urine_creat * 8.84
    # in the US unit there is only a 10^-2 difference in unit instead of 10^-3 as is the European notation. Therefore the value was adjusted, hence the 8.84 instead of 88.4
  }

  # define sex dependent variable ___________________________________
  k <- ifelse(sex == "Male", 0.9, 0.7)
  beta_cg <- ifelse(sex == "Male", 1.23, 1.04)
  alpha_2009 <- ifelse(sex == "Male", -0.411, -0.329) # CKD EPI 2009 alpha parameter
  alpha_2021 <- ifelse(sex == "Male", -0.302, -0.241) # CKD EPI 2021 alpha parameter
  ckd_female_corr <- ifelse(sex == "Male", 1, 1.018)
  ckd_ethnic_correction <- ifelse(ethnicity == "African", 1.159, 1)

  mdrd_female_corr <- ifelse(sex == "Male", 1, 1.212)
  mdrd_ethnic_correction <- ifelse(ethnicity == "African", 0.742, 1)

  # Calculate output ________________________________________________
  # Cockcroft
  if (formula == "CG") {
    out <- beta_cg * (140 - age) * (weight / creat_micromol)
  }

  # MDRD
  if (formula == "MDRD") {
    out <- 175 * (creat_mgdl)^-1.154 * ifelse(age == 0, 1, age)^-0.203 * mdrd_female_corr * mdrd_ethnic_correction
  }

  # CKD EPI 2009
  if (formula == "CKD_2009") {
    out <- 141 * min(creat_mgdl / k, 1)^alpha_2009 * max(creat_mgdl / k, 1)^-1.209 * 0.993^age * ckd_female_corr * ckd_ethnic_correction
  }

  # CKD EPI 2021
  if (formula == "CKD_2021") {
    out <- 142 * min(creat_mgdl / k, 1)^alpha_2021 * max(creat_mgdl / k, 1)^-1.20 * 0.993^age * ckd_female_corr
  }

  # UV/P formula
  if (formula == "UVP") {
    out <- urine_creat_mmol * urine_output / creat_micromol / (24 * 60) * 1000 # 1000/(24*60) = conversion factor for 24h -> min and L in mL
  }

  # Schwartz formula for kids aged 1 - 18 years
  if (formula == "schwartz") {
    k <- ifelse(age < 12, 0.55, 0.7)
    out <- k * height / creat_mgdl
  }

  # European Kidney Function Consortium (EKFC) formula
  if (formula == "EKFC") {
    exponent <- ifelse((creat_mgdl / k) < 1, -0.322, -1.132)
    age_exponent <- ifelse(age > 40, (age - 40), 0)
    out <- 107.3 * (creat_mgdl / k)^exponent * 0.990^age_exponent
  }

  # if none is required for the model (in case the model only has non renal clearance)
  if (formula == "none") {
    out <- 1
  }

  return(round(out, digits = 1))
}
