library(testthat)

# Test suite for create_patient_data

test_that("create_patient_data produces correct structure", {
  sex <- "Male"
  age <- 30
  weight <- 70
  height <- 175
  creatinine <- 100
  creat_unit <- "uM/L"
  weight_unit <- "kg"

  result <- create_patient_data(sex, age, weight, height, creatinine, creat_unit, weight_unit)

  # Check the structure of the result
  expect_type(result, "list")
  expect_named(result, c("sex", "age", "bmi", "weight", "renal_function"))

  expect_type(result$weight, "list")
  expect_named(result$weight, c("tbw", "lbw", "ajbw", "ibw"))

  expect_type(result$renal_function, "list")
  expect_named(
    result$renal_function,
    c("cg_tbw", "cg_ajbw", "cg_ibw", "cg_lbw", "mdrd", "ckd_2009", "ckd_2021", "schwartz")
  )
})


# Test calculations of BMI and derived weights
test_that("create_patient_data calculates BMI and weights correctly", {
  sex <- "Female"
  age <- 25
  weight <- 60
  height <- 165
  creatinine <- 80
  creat_unit <- "uM/L"
  weight_unit <- "kg"

  result <- create_patient_data(sex, age, weight, height, creatinine, creat_unit, weight_unit)

  # Check BMI calculation
  expect_equal(result$bmi, round(weight / (height / 100)^2, digits = 1))

  # Check derived weights
  lbw <- weight_formula(weight, height, sex, weight_unit, formula = "LBW")
  ajbw <- weight_formula(weight, height, sex, weight_unit, formula = "AJBW")
  ibw <- weight_formula(weight, height, sex, weight_unit, formula = "IBW")

  expect_equal(result$weight$lbw, lbw)
  expect_equal(result$weight$ajbw, ajbw)
  expect_equal(result$weight$ibw, ibw)
})

# Test renal function outputs
test_that("create_patient_data calculates renal function correctly", {
  sex <- "Male"
  age <- 45
  weight <- 85
  height <- 180
  creatinine <- 1.2
  creat_unit <- "mg/dL"
  weight_unit <- "lbs"

  result <- create_patient_data(sex, age, weight, height, creatinine, creat_unit, weight_unit)

  # Check Cockcroft-Gault formulas
  cg_tbw <- renal_function(sex, age, weight, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_ajbw <- renal_function(sex, age, result$weight$ajbw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_ibw <- renal_function(sex, age, result$weight$ibw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_lbw <- renal_function(sex, age, result$weight$lbw, height, creatinine, formula = "CG", creat_unit = creat_unit)

  expect_equal(result$renal_function$cg_tbw, cg_tbw)
  expect_equal(result$renal_function$cg_ajbw, cg_ajbw)
  expect_equal(result$renal_function$cg_ibw, cg_ibw)
  expect_equal(result$renal_function$cg_lbw, cg_lbw)

  # Check MDRD
  mdrd <- renal_function(sex, age, weight, height, creatinine, formula = "MDRD", creat_unit = creat_unit)
  expect_equal(result$renal_function$mdrd, mdrd)

  # Check CKD formulas
  ckd_2009 <- renal_function(sex, age, weight, height, creatinine, formula = "CKD_2009", creat_unit = creat_unit)
  ckd_2021 <- renal_function(sex, age, weight, height, creatinine, formula = "CKD_2021", creat_unit = creat_unit)

  expect_equal(result$renal_function$ckd_2009, ckd_2009)
  expect_equal(result$renal_function$ckd_2021, ckd_2021)
})

# # Edge cases
# test_that("create_patient_data handles edge cases", {
#   # Very low and high values
#   expect_error(create_patient_data("Unknown", 30, 70, 175, 100, "uM/L", "kg"))
#   expect_warning(create_patient_data("Male", 0, 70, 175, 100, "uM/L", "kg"))
#   expect_warning(create_patient_data("Female", -5, 70, 175, 100, "uM/L", "kg"))

#   # Extremely high BMI
#   result <- create_patient_data("Male", 50, 200, 150, 90, "mg/dL", "kg")
#   expect_true(result$bmi > 50)

#   # Extremely low height
#   result <- create_patient_data("Female", 25, 50, 120, 70, "uM/L", "kg")
#   expect_true(result$bmi > 20)
# })
