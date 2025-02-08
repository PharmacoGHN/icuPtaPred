biological <- list(
  cg_tbw = 70,  # Example total body weight
  cg_lbw = 60,  # Example lean body weight
  tbw = 70,     # Example total body weight
  schwartz = 120 # Example Schwartz value
)

test_that("get_model_parameters returns correct CL values", {
  expect_equal(get_model_parameters("klastrup_2020", biological)$cl, 2.25 + 0.119 * biological$cg_tbw)
  expect_equal(get_model_parameters("an_2023", biological)$cl, 4)
  expect_equal(get_model_parameters("cacqueray_2022", biological)$cl, 1.21 * (biological$tbw / 9) ^ 0.75 * (biological$schwartz / 153) ^ 0.37)
  expect_equal(get_model_parameters("guohua_2023", biological)$cl, 0.526 + 2 * biological$cg_lbw / 54)
  expect_equal(get_model_parameters("unknown_model", biological)$cl, 0)
})

test_that("get_model_parameters returns correct eta_cl values", {
  expect_equal(get_model_parameters("klastrup_2020", biological)$eta_cl, 0.533)
  expect_equal(get_model_parameters("an_2023", biological)$eta_cl, 1)
  expect_equal(get_model_parameters("caqueray_2022", biological)$eta_cl, 0.39)
  expect_equal(get_model_parameters("guohua_2023", biological)$eta_cl, 0.293)
  expect_equal(get_model_parameters("unknown_model", biological)$eta_cl, 0)
})

test_that("get_model_parameters returns correct dose_increment values", {
  expect_equal(get_model_parameters("klastrup_2020", biological, "amoxicillin")$dose_increment, 500)
  expect_equal(get_model_parameters("klastrup_2020", biological, "cefepim")$dose_increment, 1000)
  expect_equal(get_model_parameters("klastrup_2020", biological, "cefazoline")$dose_increment, 500)
  expect_equal(get_model_parameters("klastrup_2020", biological, "cefotaxim")$dose_increment, 500)
  expect_equal(get_model_parameters("klastrup_2020", biological, "ceftazidim")$dose_increment, 1000)
  expect_equal(get_model_parameters("klastrup_2020", biological, "ceftaroline")$dose_increment, 1000)
  expect_equal(get_model_parameters("klastrup_2020", biological, "ceftobiprol")$dose_increment, 1000)
  expect_equal(get_model_parameters("klastrup_2020", biological, "pipetazo")$dose_increment, 2000)
  expect_equal(get_model_parameters("klastrup_2020", biological, "meropenem")$dose_increment, 500)
  expect_equal(get_model_parameters("klastrup_2020", biological, "unknown_drug")$dose_increment, 0)
})