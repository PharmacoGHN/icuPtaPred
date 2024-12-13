library(testthat)

# Test suite for get_model_parameters

# Mock patient data
patient_data <- list(
  sex = "Male",
  age = 45,
  bmi = 25.0,
  weight = list(
    tbw = 70,
    lbw = 65,
    ajbw = 68,
    ibw = 66
  ),
  renal_function = list(
    cg_tbw = 90,
    cg_lbw = 85,
    cg_ibw = 88,
    cg_ajbw = 87,
    mdrd = 95,
    ckd_2009 = 92,
    ckd_2021 = 94,
    schwartz = 89
  )
)

# Mock model bank
model_bank <- list(
  amoxicillin = list(NA),
  cefepim = list(
    cacqueray_2022 = list(
      tvcl = "1.21 * (tbw / 9) ^ 0.75 * (schwartz / 153) ^ 0.37",
      eta_cl = 0.39
    )
  ),
  cefotaxim = list(NA),
  meropenem = list(NA),
  piptazo = list(NA)
)

test_that("get_model_parameters extracts correct parameters", {
  drug <- "cefepim"
  model <- "cacqueray_2022"

  result <- get_model_parameters(patient_data, drug, model, model_bank)

  # Expected values
  tvcl_expected <- 1.21 * (patient_data$weight$tbw / 9) ^ 0.75 * (patient_data$renal_function$schwartz / 153) ^ 0.37
  eta_cl_expected <- 0.39

  # Validate the output
  expect_type(result, "list")
  expect_named(result, c("tvcl", "eta_cl"))

  # Check calculated values
  expect_equal(result$tvcl, tvcl_expected)
  expect_equal(result$eta_cl, eta_cl_expected)
})

# Test handling of missing drug or model

# test_that("get_model_parameters handles missing drug or model", {
#   expect_error(get_model_parameters(patient_data, "unknown_drug", "unknown_model", model_bank), "object of type 'closure' is not subsettable")
#   expect_error(get_model_parameters(patient_data, "cefepim", "unknown_model", model_bank), "object of type 'closure' is not subsettable")
# })

# # Test edge cases
# test_that("get_model_parameters handles edge cases", {
#   # Missing renal function
#   incomplete_patient_data <- patient_data
#   incomplete_patient_data$renal_function <- NULL

#   expect_error(get_model_parameters(incomplete_patient_data, "cefepim", "cacqueray_2022", model_bank), "attempt to select less than one element in get1index")

#   # Missing weight data
#   incomplete_patient_data <- patient_data
#   incomplete_patient_data$weight <- NULL

#   expect_error(get_model_parameters(incomplete_patient_data, "cefepim", "cacqueray_2022", model_bank), "object of type 'closure' is not subsettable")
# })
