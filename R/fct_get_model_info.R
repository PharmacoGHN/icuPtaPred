#' create_patient_data
#'
#' @description a fct that create an object of classe patient.data.
#' The output renal function based on age (adult or children) and all weight
#' and creatinine clearance descriptor that can be used to calculate
#' drug specific clearance based on model input
#'
#' @param sex to comment
#' @param age to comment
#' @param weight to comment
#' @param height to comment
#' @param creatinine to comment
#' @param creat_unit to comment
#' @param weight_unit to comment
#'
#' @return patient_data
#'
#' @noRd
#'
create_patient_data <- function(
  sex,
  age,
  weight,
  height,
  creatinine,
  creat_unit,
  weight_unit
) {

  lbw <- weight_formula(weight, height, sex, weight_unit, formula = "LBW")
  ajbw <- weight_formula(weight, height, sex, weight_unit, formula = "AJBW")
  ibw <- weight_formula(weight, height, sex, weight_unit, formula = "IBW")

  patient_data <- list(
    sex = sex,
    age = age,
    bmi = round(weight / (height / 100) ^ 2, digits = 1),
    weight =  list(
      tbw = weight,
      lbw = lbw,
      ajbw = ajbw,
      ibw = ibw
    ),
    renal_function = list(
      cg_tbw = renal_function(sex, age, weight, height, creatinine, formula = "CG", creat_unit = creat_unit),
      cg_ajbw = renal_function(sex, age, ajbw, height, creatinine, formula = "CG", creat_unit = creat_unit),
      cg_ibw = renal_function(sex, age, ibw, height, creatinine, formula = "CG", creat_unit = creat_unit),
      cg_lbw = renal_function(sex, age, lbw, height, creatinine, formula = "CG", creat_unit = creat_unit),
      mdrd = renal_function(sex, age, weight, height, creatinine, formula = "MDRD", creat_unit = creat_unit),
      ckd_2009 = renal_function(sex, age, weight, height, creatinine, formula = "CKD_2009", creat_unit = creat_unit),
      ckd_2021 = renal_function(sex, age, weight, height, creatinine, formula = "CKD_2021", creat_unit = creat_unit),
      schwartz = renal_function(sex, age, weight, height, creatinine, formula = "schwartz", creat_unit = creat_unit)
    )
  )

  return(patient_data)
}


#' get_model_parameters
#'
#' @description A fct that call PK model and calculate specific clearance
#' based on given patient characteristics
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

get_model_parameters <- function(patient_data, drug, model, model_bank) {

  #get patient_info and shorten data
  d <- patient_data

  #get all available parameters
  sex <- d[["sex"]]
  age <- d[["age"]]
  bmi <- d[["bmi"]]
  tbw <- d[["weight"]][["tbw"]]
  lbw <- d[["weight"]][["lbw"]]
  ajbw <- d[["weight"]][["ajbw"]]
  ibw <- d[["weight"]][["ibw"]]
  cg_tbw <- d[["renal_function"]][["cg_tbw"]]
  cg_lbw <- d[["renal_function"]][["cg_lbw"]]
  cg_ibw <- d[["renal_function"]][["cg_ibw"]]
  cg_ajbw <- d[["renal_function"]][["cg_ajbw"]]
  mdrd <- d[["renal_function"]][["mdrd"]]
  ckd_2009 <- d[["renal_function"]][["ckd_2009"]]
  ckd_2021 <- d[["renal_function"]][["ckd_2021"]]
  schwartz <- d[["renal_function"]][["schwartz"]]

  # get model parameters and let possibility to add other parameters if needed
  model_parameters <- list(
    tvcl = eval(parse(text = model_bank[[drug]][[model]][["tvcl"]])),
    eta_cl = model_bank[[drug]][[model]][["eta_cl"]]
  )
  return(model_parameters)
}