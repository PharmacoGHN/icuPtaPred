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

  lbw <- weight_formula(weight, height, sex, weight_unit, formula = "")
  ajbw <- weight_formula(weight, height, sex, weight_unit, formula = "")
  ibw <- weight_formula(weight, height, sex, weight_unit, formula = "")

  patient_data <- list(
    sex = sex,
    age = age,
    bmi = weight / (height ^ 2),
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

  # create the permanent object class patient.data

  return(patient_data)
}


#' get_model_info
#'
#' @description A fct that call PK model and calculate specific clearance
#' based on given patient characteristics
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

get_model_info <- function(patient_data, model) {
  
}