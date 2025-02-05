# Test sim_concentration function
test_that("sim_concentration returns expected output", {
  dose <- 1000
  tvcl <- 10
  eta_cl <- 2
  quantile <- c(0.025, 0.975)
  mic <- c(0.125, 0.25, 0.5, 1, 2, 4, 8)

  expected_sim_conc_df <- as.data.frame(
    tibble::tribble(
      ~css_mic, ~mic, ~percentile_2.5, ~percentile_97.5,
      33.3,     0.1,   24.0,            54.9,
      16.7,     0.2,   12.0,            27.5,
       8.3,     0.5,    6.0,            13.7,
       4.2,     1.0,    3.0,             6.9,
       2.1,     2.0,    1.5,             3.4,
       1.0,     4.0,    0.7,             1.7,
       0.5,     8.0,    0.4,             0.9
    )
  )

  result <- sim_concentration(dose, tvcl, eta_cl, quantile, mic)

  # Check that the result is a dataframe
  expect_s3_class(result, "data.frame")

  # Check column names
  expect_equal(colnames(result), c("css_mic", "mic", "percentile_2.5", "percentile_97.5"))

  # Check that the number of rows matches mic length
  expect_equal(nrow(result), length(mic))

  # Check that values are numeric
  expect_type(result$css_mic, "double")
  expect_type(result$mic, "double")
  expect_type(result$percentile_2.5, "double")
  expect_type(result$percentile_97.5, "double")

  # Check if values make sense (e.g., css_mic should be decreasing as MIC increases)
  expect_true(all(diff(result$css_mic) < 0))

  # Check if percentile_2.5 is always lower than percentile_97.5
  expect_true(all(result$percentile_2.5 < result$percentile_97.5))

  # Check if data return as the one expected (data have a seed embeded in function)
  expect_equal(result, expected_sim_conc_df)
})
