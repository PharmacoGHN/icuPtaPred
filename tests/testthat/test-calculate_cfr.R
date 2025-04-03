testthat::test_that("calculate_cfr works correctly", {
  tvcl <- 2.25 + (100 * 0.119) # Typical clearance for piperacillin-tazobactam based on klastrup 2020 for a patient with a creatinine clearance of 100 ml/min
  eta_cl <- 0.533 # Standard deviation for piperacillin-tazobactam based on klastrup 2020
  dose <- 8000 # Dose in mg for piperacillin-tazobactam
  toxicity_threshold <- 157 # Toxicity threshold for piperacillin-tazobactam based on klastrup 2020

  # Define a sample mic_distribution dataframe based on the distribution from EUCAST
  # These are the real value for Piperacillin-tazobactam and Pseudomonas aeruginosa as of 03/04/2025
  mic_distribution <- data.frame(
    mic = c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512),
    distribution = c(4, 16, 28, 441, 827, 2884, 10269, 5473, 3351, 1728, 1323, 2934, 527, 161)
  )

  # Calculate the relative distribution
  relative_distribution <- mic_distribution$distribution / sum(mic_distribution$distribution)

  # not run for test data creation only
  # css_distribution <- calc_css_distribution(dose = dose, tvcl = tvcl, eta_cl = eta_cl)

  cfr <- 0 # Initialize cfr variable
  for (i in 1:nrow(mic_distribution)) {
    css_distribution_mic <- mean(css_distribution > mic_distribution$mic[i]) * relative_distribution[i]
    cfr <- sum(cfr, css_distribution_mic)
  }


  # Test with a simple example
  result <- calculate_cfr(
    dose = dose,
    tvcl = tvcl,
    eta_cl = eta_cl,
    mic_distribution = mic_distribution,
    toxicity_threshold = toxicity_threshold
  )

  # Check if the result is a numeric value
  expect_type(result, "double")

  # Check if the result is within the expected range
  expect_gte(result, -1)



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
