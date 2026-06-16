box::use(
  testthat[expect_equal, expect_error, expect_false, expect_true, test_that]
)

box::use(
  app/logic/model_registry
)

impl <- attr(model_registry, "namespace")

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
  result <- model_registry$get_model_parameters("Buning_2021", biological) # CL = 3.42 * (biological$ckd_2009 / 73)^0.772
  expect_equal(result$cl, 3.31, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.35, tolerance = 0.01)

  result_2 <- model_registry$get_model_parameters("Buning_2021", biological_2)
  expect_equal(result_2$cl, 3.85, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.35, tolerance = 0.01)
})

test_that("get_model_parameters returns correct values for Launay_2024", {
  result <- model_registry$get_model_parameters("Launay_2024", biological) # CL = 4.45 * (biological$ckd_2009 / 73.9)^0.9
  expect_equal(result$cl, 4.24, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.46)

  result_2 <- model_registry$get_model_parameters("Launay_2024", biological_2)
  expect_equal(result_2$cl, 5.04, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.46)
})

test_that("get_model_parameters returns correct values for Cojutti_2024", {
  result <- model_registry$get_model_parameters("Cojutti_2024", biological) # CL = 5 * (biological$ekfc / 70) ^ 0.7
  expect_equal(result$cl, 5, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.616, tolerance = 0.01)

  result_2 <- model_registry$get_model_parameters("Cojutti_2024", biological_2)
  expect_equal(result_2$cl, 5.73, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.616, tolerance = 0.01)
})

# Ceftolozane model testing ______________________________________________
test_that("get_model_parameters returns correct values for Chandorkar_2015", {
  result <- model_registry$get_model_parameters("Chandorkar_2015", biological) # CL = 5.11 * 1.215 * (biological$cg_tbw / 109) ^ 0.715
  expect_equal(result$cl, 4.52, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.321, tolerance = 0.01)

  result_2 <- model_registry$get_model_parameters("Chandorkar_2015", biological_2)
  expect_equal(result_2$cl, 5.19, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.321, tolerance = 0.01)
})

test_that("get_model_parameters returns correct values for Zhang_2021", {
  result <- model_registry$get_model_parameters("Zhang_2021", biological) # CL = 4.84 * (biological$cg_tbw / 100) ^ 0.701
  expect_equal(result$cl, 3.77, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.411, tolerance = 0.01)

  result_2 <- model_registry$get_model_parameters("Zhang_2021", biological_2)
  expect_equal(result_2$cl, 4.32, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.411, tolerance = 0.01)
})

# Cefepime model testing ______________________________________________
test_that("get_model_parameters returns correct values for Cacqueray_2022", {
  result <- model_registry$get_model_parameters("Cacqueray_2022", biological) #CL = 1.21 * (biological$tbw / 9) ^ 0.75 * (biological$schwartz / 153) ^ 0.37)
  expect_equal(result$cl, 5.15, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.39)

  result_2 <- model_registry$get_model_parameters("Cacqueray_2022", biological_2)
  expect_equal(result_2$cl, 6.47, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.39)
})

test_that("get_model_parameters returns correct values for An_2023", {
  result <- model_registry$get_model_parameters("An_2023", biological)  # Clearance  0.526 + 2 * biological$cg_lbw / 54
  expect_equal(result$cl, 2.748, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.293, tolerance = 0.01)

  result_2 <- model_registry$get_model_parameters("An_2023", biological_2)
  expect_equal(result_2$cl, 3.303, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.293, tolerance = 0.01)
})

# Meropenem model testing ______________________________________________
test_that("get_model_parameters returns correct values for Fukumoto_2023", {
  result <- model_registry$get_model_parameters("Fukumoto_2023", biological) # CL  = 1.35 * ((biological$uvp * 1.73 / biological$bsa) / 87.6)^0.67)
  expect_equal(result$cl, 0.950, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.218, tolerance = 0.01)

  result_2 <- model_registry$get_model_parameters("Fukumoto_2023", biological_2)
  expect_equal(result_2$cl, 1.036, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.218, tolerance = 0.01)
})

# Piperacillin model testing ______________________________________________
test_that("get_model_parameters returns correct values for Klastrup_2020", {
  result <- model_registry$get_model_parameters("Klastrup_2020", biological) # Clearance klastrup 2.25 + 0.119 * biological$cg_tbw
  expect_equal(result$cl, 10.58, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.533, tolerance = 0.01)

  result_2 <- model_registry$get_model_parameters("Klastrup_2020", biological_2)
  expect_equal(result_2$cl, 12.365, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.533, tolerance = 0.01)
})

test_that("get_model_parameters returns correct values for Sukarnjanaset_2019", {
  result <- model_registry$get_model_parameters("Sukarnjanaset_2019", biological) # Clearance Sukarnjanaset 5.37 + (0.06 * (biological$cg_tbw - 55))
  expect_equal(result$cl, 6.27, tolerance = 0.001)
  expect_equal(result$eta_cl, 0.279, tolerance = 0.01)

  result_2 <- model_registry$get_model_parameters("Sukarnjanaset_2019", biological_2)
  expect_equal(result_2$cl, 7.17, tolerance = 0.001)
  expect_equal(result_2$eta_cl, 0.279, tolerance = 0.01)
})

test_that("get_model_parameters returns correct values for Udy_2015", {
  result <- model_registry$get_model_parameters("Udy_2015", biological) # Clearance Udy 16.3 * (biological$cg_tbw / 100)
  expect_equal(result$cl, 11.41, tolerance = 0.01)
  expect_equal(result$eta_cl, 0.61, tolerance = 0.01)

  result_2 <- model_registry$get_model_parameters("Udy_2015", biological_2)
  expect_equal(result_2$cl, 13.85, tolerance = 0.01)
  expect_equal(result_2$eta_cl, 0.61, tolerance = 0.01)
})

# Default model testing ______________________________________________
test_that("get_model_parameters errors for unknown model", {
  expect_error(model_registry$get_model_parameters("unknown_model", biological))
  expect_error(model_registry$get_model_parameters("unknown_model", biological_2))
})


# Test dose increment and toxicity threshold ______________________________________________
test_that("get_model_parameters returns registry dose and safety metadata", {
  cefepime_param <- model_registry$get_model_parameters("An_2023", biological, "Cefepime")
  expect_equal(cefepime_param$dose_increment, 1.000)
  expect_equal(cefepime_param$max_dose, 20)
  expect_equal(cefepime_param$toxicity_threshold, 20)
  expect_true(is.na(cefepime_param$fu))
  expect_false(cefepime_param$is_not_available)
  expect_equal(cefepime_param$availability_message, "")

  ceftazidime_param <- model_registry$get_model_parameters("Buning_2021", biological, "Ceftazidime")
  expect_equal(ceftazidime_param$dose_increment, 1.000)
  expect_equal(ceftazidime_param$max_dose, 20)
  expect_true(is.na(ceftazidime_param$toxicity_threshold))

  ceftolozane_param <- model_registry$get_model_parameters("Zhang_2021", biological, "Ceftolozane")
  expect_equal(ceftolozane_param$dose_increment, 1.000)
  expect_equal(ceftolozane_param$max_dose, 20)
  expect_true(is.na(ceftolozane_param$toxicity_threshold))

  meropenem_param <- model_registry$get_model_parameters("Ehrmann_2019", biological, "Meropenem")
  expect_equal(meropenem_param$dose_increment, 0.500)
  expect_equal(meropenem_param$max_dose, 20)
  expect_equal(meropenem_param$toxicity_threshold, 45)

  piperacillin_param <- model_registry$get_model_parameters("Klastrup_2020", biological, "Piperacillin-tazobactam")
  expect_equal(piperacillin_param$dose_increment, 2.000)
  expect_equal(piperacillin_param$max_dose, 40)
  expect_equal(piperacillin_param$toxicity_threshold, 157)

  expect_error(model_registry$get_model_parameters("Klastrup_2020", biological, "unknown_drug"))
})

test_that("get_model_default returns the right model", {
  expect_equal(model_registry$get_default_model("Cefepime"), "An_2023")
  expect_equal(model_registry$get_default_model("Ceftazidime"), "Buning_2021")
  expect_equal(model_registry$get_default_model("Ceftolozane"), "Zhang_2021")
  expect_equal(model_registry$get_default_model("Cefiderocol"), "Zhar_2022")
  expect_equal(model_registry$get_default_model("Piperacillin-tazobactam"), "Klastrup_2020")
  expect_equal(model_registry$get_default_model("Meropenem"), "Ehrmann_2019")
})

test_that("coerce_registry backfills availability metadata for legacy rows", {
  registry <- impl$coerce_registry(
    data.frame(
      drug = "Meropenem",
      model = "Legacy_Test_Model",
      is_default = FALSE,
      dose_increment = 0.5,
      max_dose = 20,
      toxicity_threshold = 45,
      renal_metric = "none",
      renal_formula = "No renal formula",
      clearance_expr = "1",
      eta_cl_expr = "1",
      stringsAsFactors = FALSE
    )
  )

  expect_false(registry$is_not_available[[1]])
  expect_equal(registry$availability_message[[1]], "")
  expect_true(is.na(registry$fu[[1]]))
})

test_that("availability metadata survives registry JSON roundtrip", {
  registry <- impl$coerce_registry(
    data.frame(
      drug = "Meropenem",
      model = "Availability_Test_Model",
      is_default = FALSE,
      is_not_available = TRUE,
      availability_message = "Not yet validated for routine use.",
      dose_increment = 0.5,
      max_dose = 20,
      toxicity_threshold = 45,
      fu = 0.65,
      renal_metric = "none",
      renal_formula = "No renal formula",
      clearance_expr = "1",
      eta_cl_expr = "1",
      stringsAsFactors = FALSE
    )
  )
  registry_path <- tempfile(fileext = ".json")
  on.exit(unlink(registry_path), add = TRUE)

  impl$write_registry_json_file(registry_path, registry)
  roundtrip_registry <- impl$read_registry_json_file(registry_path)

  expect_true(roundtrip_registry$is_not_available[[1]])
  expect_equal(roundtrip_registry$availability_message[[1]], "Not yet validated for routine use.")
  expect_equal(roundtrip_registry$fu[[1]], 0.65)
})