biological <- list(
  cg_tbw = 70,  # Example total body weight
  cg_lbw = 60,  # Example lean body weight
  tbw = 70,     # Example total body weight
  uvp = 60,
  bsa = 2,
  schwartz = 120 # Example Schwartz value
)

biological_2 <- list(
  cg_tbw = 85,  # Different total body weight
  cg_lbw = 75,  # Different lean body weight
  tbw = 85,     # Different total body weight
  uvp = 75,
  bsa = 2.2,
  schwartz = 150 # Different Schwartz value
)

# Ceftazidim model testing

# Ceftolozane model testing

# Cefepim model testing
test_that("get_model_parameters returns correct values for an_2023", {
  result <- get_model_parameters("an_2023", biological) # CL not implemented yet
  expect_equal(result$cl, 4)
  expect_equal(result$eta_cl, 0.2926, tolerance = 1e-3)

  result_2 <- get_model_parameters("an_2023", biological_2)
  expect_equal(result_2$cl, 4)
  expect_equal(result_2$eta_cl, 0.2926, tolerance = 1e-3)
})

test_that("get_model_parameters returns correct values for cacqueray_2022", {
  result <- get_model_parameters("cacqueray_2022", biological) #CL = 1.21 * (biological$tbw / 9) ^ 0.75 * (biological$schwartz / 153) ^ 0.37)
  expect_equal(result$cl, 5.15, tolerance = 0.01) 
  expect_equal(result$eta_cl, 0.39)

  result_2 <- get_model_parameters("cacqueray_2022", biological_2)
  expect_equal(result_2$cl, 6.47, tolerance = 0.01) 
  expect_equal(result_2$eta_cl, 0.39)
})

test_that("get_model_parameters returns correct values for guohua_2023", {
  result <- get_model_parameters("guohua_2023", biological)  # Clearance  0.526 + 2 * biological$cg_lbw / 54
  expect_equal(result$cl, 2.748, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.293)

  result_2 <- get_model_parameters("guohua_2023", biological_2)
  expect_equal(result_2$cl, 3.303, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.293)
})

# Meropenem model testing
test_that("get_model_parameters returns correct values for Fukumoto_2023", {
  result <- get_model_parameters("Fukumoto_2023", biological) # CL  = 1.35 * ((biological$uvp * 1.73 / biological$bsa) / 87.6)^0.67)
  expect_equal(result$cl, 0.950, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.218, tolerance = 0.01)

  result_2 <- get_model_parameters("Fukumoto_2023", biological_2)
  expect_equal(result_2$cl, 1.036, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.218, tolerance = 0.01)
})

# Piperacillin model testing
test_that("get_model_parameters returns correct values for klastrup_2020", {
  result <- get_model_parameters("klastrup_2020", biological) # Clearance klastrup 2.25 + 0.119 * biological$cg_tbw
  expect_equal(result$cl, 10.58, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.533)

  result_2 <- get_model_parameters("klastrup_2020", biological_2)
  expect_equal(result_2$cl, 12.365, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.533)
})

# Default model testing
test_that("get_model_parameters returns default values for unknown model", {
  result <- get_model_parameters("unknown_model", biological)
  expect_equal(result$cl, 0)
  expect_equal(result$eta_cl, 0)

  result_2 <- get_model_parameters("unknown_model", biological_2)
  expect_equal(result_2$cl, 0)
  expect_equal(result_2$eta_cl, 0)
})

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
