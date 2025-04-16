testthat::test_that("calculate_cfr works correctly", {

  # Parameters definition
  tvcl <- 2.25 + (100 * 0.119) # Typical clearance for piperacillin-tazobactam based on klastrup 2020 for a patient with a creatinine clearance of 100 ml/min
  eta_cl <- 0.533 # Standard deviation for piperacillin-tazobactam based on klastrup 2020
  dose <- 8000 # Dose in mg for piperacillin-tazobactam
  toxicity_threshold <- 157 # Toxicity threshold for piperacillin-tazobactam based on klastrup 2020
  dose_increment <- 2000 # Dose increment in mg for piperacillin-tazobactam
  dose_max <- 32000 # Maximum dose in mg for piperacillin-tazobactam
  dosing_sequence <- seq(0, dose_max, by = dose_increment) # Dosing sequence from 0 to 32g with a step of 2g
  dosing_sequence_css <- dosing_sequence / 24 / tvcl # Dosing sequence in css for piperacillin-tazobactam
  css_nsim0 <- dose / tvcl / 24



  # Define a sample mic_distribution dataframe based on the distribution from EUCAST
  # These are the real value for Piperacillin-tazobactam and Pseudomonas aeruginosa as of 03/04/2025
  mic_distribution <- data.frame(
    mic = c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512),
    distribution = c(4, 16, 28, 441, 827, 2884, 10269, 5473, 3351, 1728, 1323, 2934, 527, 161)
  ) |>
    dplyr::mutate(relative_distribution = distribution / sum(distribution))

  #expeted cfr value for piperacillin-tazobactam and pseudomonas aeruginosa
  rank_css_nsim0 <- min(which(mic_distribution$mic > css_nsim0))
  expected_cfr_nsim0 <- 1 - sum(mic_distribution$relative_distribution[rank_css_nsim0:14])

  # Calculate expected CFR for multiple doses using the same approach as above
  expected_cfr_multiple_dose <- numeric(length(dosing_sequence_css))

  for (i in seq_along(dosing_sequence_css)) {
    css <- dosing_sequence_css[i]
    if (css == 0) {
      expected_cfr_multiple_dose[i] <- 0  # No drug, no effect
    } else {
      rank_css <- min(which(mic_distribution$mic > css))
      expected_cfr_multiple_dose[i] <- 1 - sum(mic_distribution$relative_distribution[rank_css:14])
    }
  }


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
  expect_equal(result_nsim0$cfr, expected_cfr_nsim0, tolerance = 0.001)
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
  expect_type(result, "list")
  expect_true("cfr" %in% names(result))
  expect_true("toxicity_proportion" %in% names(result))
  expect_type(result$cfr, "double")
  expect_type(result$toxicity_proportion, "double")


  result_cfr_multiple_dose_expected <- calculate_cfr_mulitple_doses(
    dose_increment = dose_increment,
    dose_max = dose_max,
    tvcl = tvcl,
    eta_cl = eta_cl,
    mic_distribution = mic_distribution,
    toxicity_threshold = toxicity_threshold,
    n_sim = 0
  )
  # Check if the result is a list containing cfr and toxicity_proportion
  expect_s3_class(result_cfr_multiple_dose_expected, "data.frame")
  expect_equal(nrow(result_cfr_multiple_dose_expected), length(dosing_sequence_css))
  expect_equal(ncol(result_cfr_multiple_dose_expected), 3)
  expect_type(result_cfr_multiple_dose_expected$cfr, "double")
  expect_type(result_cfr_multiple_dose_expected$toxicity_proportion, "double")
  expect_equal(result_cfr_multiple_dose_expected$cfr, expected_cfr_multiple_dose, tolerance = 0.01)
  #expect_true("cfr" %in% names(result_cfr_multiple_dose))
})
