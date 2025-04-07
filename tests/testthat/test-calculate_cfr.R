testthat::test_that("calculate_cfr works correctly", {

  # Parameters definition
  tvcl <- 2.25 + (100 * 0.119) # Typical clearance for piperacillin-tazobactam based on klastrup 2020 for a patient with a creatinine clearance of 100 ml/min
  eta_cl <- 0.533 # Standard deviation for piperacillin-tazobactam based on klastrup 2020
  dose <- 8000 # Dose in mg for piperacillin-tazobactam
  toxicity_threshold <- 157 # Toxicity threshold for piperacillin-tazobactam based on klastrup 2020

  # Define a sample mic_distribution dataframe based on the distribution from EUCAST
  # These are the real value for Piperacillin-tazobactam and Pseudomonas aeruginosa as of 03/04/2025
  mic_distribution <- data.frame(
    mic = c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512),
    distribution = c(4, 16, 28, 441, 827, 2884, 10269, 5473, 3351, 1728, 1323, 2934, 527, 161)
  ) |>
    dplyr::mutate(relative_distribution = distribution / sum(distribution)
  )

  mic_distribution$relative_distribution <- mic_distribution$distribution / sum(mic_distribution$distribution)


  #expeted cfr value for piperacillin-tazobactam and pseudomonas aeruginosa
  expected_cfr_nsim0 <- 1 - sum(mic_distribution$relative_distribution[10:14]) # 0.9999999


  # test section ______________________
  # Test with a simple example
  result_nsim0 <- calculate_cfr(
    dose = dose,
    tvcl = tvcl,
    eta_cl = eta_cl,
    mic_distribution = mic_distribution,
    toxicity_threshold = toxicity_threshold,
    n_sim = 0
  )

  # Check if the result is a numeric value
  expect_type(result_nsim0, "list")
  expect_type(result_nsim0$cfr, "double")
  expect_type(result_nsim0$toxicity_proportion, "double")
  expect_equal(result_nsim0$cfr, expected_cfr_nsim0, tolerance = 0.01)
  expect_equal(result_nsim0$toxicity_proportion, 0, tolerance = 0.01)



  # Test with a larger dose and check for toxicity threshold
  result <- calculate_cfr(
    dose = 16,
    tvcl = tvcl,
    eta_cl = eta_cl,
    mic_distribution = mic_distribution,
    toxicity_threshold = toxicity_threshold
  )

  # Check if the result is a list containing cfr and toxicity_proportion
  expect_type(result$cfr, "double")
})
