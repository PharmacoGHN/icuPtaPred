biological <- list(
  cg_tbw = 70,  # Example total body weight
  cg_lbw = 60,  # Example lean body weight
  tbw = 70,     # Example total body weight
  uvp = 60,
  ckd_2009 = 70,
  ekfc = 70,
  bsa = 2,
  schwartz = 120 # Example Schwartz value
)

biological_2 <- list(
  cg_tbw = 85,  # Different total body weight
  cg_lbw = 75,  # Different lean body weight
  tbw = 85,     # Different total body weight
  uvp = 75,
  ckd_2009 = 85,
  ekfc = 85,
  bsa = 2.2,
  schwartz = 150 # Different Schwartz value
)

# Ceftazidim model testing ______________________________________________
test_that("get_model_parameters returns correct values for Buning_2021", {
  result <- get_model_parameters("Buning_2021", biological) # CL = 3.42 * (biological$ckd_2009 / 73)^0.772
  expect_equal(result$cl, 3.31, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.35, tolerance = 0.01)

  result_2 <- get_model_parameters("Buning_2021", biological_2)
  expect_equal(result_2$cl, 3.85, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.35, tolerance = 0.01)
})

test_that("get_model_parameters returns correct values for Launay_2024", {
  result <- get_model_parameters("Launay_2024", biological) # CL = 4.45 * (biological$ckd_2009 / 73.9)^0.9
  expect_equal(result$cl, 4.24, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.46)

  result_2 <- get_model_parameters("Launay_2024", biological_2)
  expect_equal(result_2$cl, 5.04, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.46)
})

test_that("get_model_parameters returns correct values for Cojutti_2024", {
  result <- get_model_parameters("Cojutti_2024", biological) # CL = 5 * (biological$ekfc / 70) ^ 0.7
  expect_equal(result$cl, 5, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.616, tolerance = 0.01)

  result_2 <- get_model_parameters("Cojutti_2024", biological_2)
  expect_equal(result_2$cl, 5.73, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.616, tolerance = 0.01)
})

# Ceftolozane model testing ______________________________________________

# Cefepim model testing ______________________________________________
test_that("get_model_parameters returns correct values for cacqueray_2022", {
  result <- get_model_parameters("cacqueray_2022", biological) #CL = 1.21 * (biological$tbw / 9) ^ 0.75 * (biological$schwartz / 153) ^ 0.37)
  expect_equal(result$cl, 5.15, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.39)

  result_2 <- get_model_parameters("cacqueray_2022", biological_2)
  expect_equal(result_2$cl, 6.47, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.39)
})

test_that("get_model_parameters returns correct values for an_2023", {
  result <- get_model_parameters("an_2023", biological)  # Clearance  0.526 + 2 * biological$cg_lbw / 54
  expect_equal(result$cl, 2.748, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.293, tolerance = 0.01)

  result_2 <- get_model_parameters("an_2023", biological_2)
  expect_equal(result_2$cl, 3.303, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.293, tolerance = 0.01)
})

# Meropenem model testing ______________________________________________
test_that("get_model_parameters returns correct values for Fukumoto_2023", {
  result <- get_model_parameters("Fukumoto_2023", biological) # CL  = 1.35 * ((biological$uvp * 1.73 / biological$bsa) / 87.6)^0.67)
  expect_equal(result$cl, 0.950, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.218, tolerance = 0.01)

  result_2 <- get_model_parameters("Fukumoto_2023", biological_2)
  expect_equal(result_2$cl, 1.036, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.218, tolerance = 0.01)
})

# Piperacillin model testing ______________________________________________
test_that("get_model_parameters returns correct values for klastrup_2020", {
  result <- get_model_parameters("klastrup_2020", biological) # Clearance klastrup 2.25 + 0.119 * biological$cg_tbw
  expect_equal(result$cl, 10.58, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.533, tolerance = 0.01)

  result_2 <- get_model_parameters("klastrup_2020", biological_2)
  expect_equal(result_2$cl, 12.365, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.533, tolerance = 0.01)
})

testthat::test_that("get_model_parameters returns correct values for Sukarnjanaset_2019", {
  result <- get_model_parameters("Sukarnjanaset_2019", biological) # Clearance Sukarnjanaset 5.37 + (0.06 * (biological$cg_tbw - 55))
  expect_equal(result$cl, 6.27, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.279, tolerance = 0.01)

  result_2 <- get_model_parameters("Sukarnjanaset_2019", biological_2)
  expect_equal(result_2$cl, 7.17, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.279, tolerance = 0.01)
})

test_that("get_model_parameters returns correct values for Udy_2015", {
  result <- get_model_parameters("Udy_2015", biological) # Clearance Udy 16.3 * (biological$cg_tbw / 100)
  expect_equal(result$cl, 11.41, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.61, tolerance = 0.01)

  result_2 <- get_model_parameters("Udy_2015", biological_2)
  expect_equal(result_2$cl, 13.85, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.61, tolerance = 0.01)
})

# Default model testing ______________________________________________
test_that("get_model_parameters returns default values for unknown model", {
  result <- get_model_parameters("unknown_model", biological)
  expect_equal(result$cl, 1)
  expect_equal(result$eta_cl, 1)

  result_2 <- get_model_parameters("unknown_model", biological_2)
  expect_equal(result_2$cl, 1)
  expect_equal(result_2$eta_cl, 1)
})


# Test dose increment and toxicity threshold ______________________________________________
test_that("get_model_parameters returns correct dose_increment values", {
  expect_equal(get_model_parameters("klastrup_2020", biological, "amoxicillin")$dose_increment, 0.500)
  expect_equal(get_model_parameters("klastrup_2020", biological, "cefepim")$dose_increment, 1.000)
  expect_equal(get_model_parameters("klastrup_2020", biological, "cefazoline")$dose_increment, 0.500)
  expect_equal(get_model_parameters("klastrup_2020", biological, "cefotaxim")$dose_increment, 0.500)
  expect_equal(get_model_parameters("klastrup_2020", biological, "ceftazidim")$dose_increment, 1.000)
  expect_equal(get_model_parameters("klastrup_2020", biological, "ceftaroline")$dose_increment, 1.000)
  expect_equal(get_model_parameters("klastrup_2020", biological, "ceftobiprol")$dose_increment, 1.000)
  expect_equal(get_model_parameters("klastrup_2020", biological, "pipetazo")$dose_increment, 2.000)
  expect_equal(get_model_parameters("klastrup_2020", biological, "meropenem")$dose_increment, 0.500)
  expect_equal(get_model_parameters("klastrup_2020", biological, "unknown_drug")$dose_increment, 0)
})

test_that("get_model_parameters returns correct toxicity_threshold values", {
  expect_equal(drug_threshold("amoxicillin"), NA)
  expect_equal(drug_threshold("cefepim"), 20)
  expect_equal(drug_threshold("cefazoline"), NA)
  expect_equal(drug_threshold("cefotaxim"), NA)
  expect_equal(drug_threshold("ceftazidim"), NA)
  expect_equal(drug_threshold("ceftaroline"), NA)
  expect_equal(drug_threshold("ceftobiprol"), NA)
  expect_equal(drug_threshold("pipetazo"), 157)
  expect_equal(drug_threshold("meropenem"), 45)
})
