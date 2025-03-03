testthat::test_that("date_time_format function correctly return the right output", {
  # Call the function to read the file
  test <- date_time_format(as.Date("2023-10-01"))

  # Test various aspects of the result
  testthat::expect_equal(date_time_format(), paste(format(Sys.Date(), "%Y/%m/%d"), format(Sys.time(), "%H:%M")))
  testthat::expect_equal(test, paste("2023/10/01", format(Sys.time(), "%H:%M")))
})

testthat::test_that("is_unique return the right output", {
  testthat::expect_false(is_unique(c(1, 1, 2, 1, 3)))
  testthat::expect_true(is_unique(c(1, 2, 3, 4, 5)))
})

testthat::test_that("get_sd_from_cv function correctly converts CV to SD", {
  # Test with known values
  testthat::expect_equal(get_sd_from_cv(0.1), sqrt(log(0.1^2 + 1)), tolerance = 1e-6)
  testthat::expect_equal(get_sd_from_cv(0.5), sqrt(log(0.5^2 + 1)), tolerance = 1e-6)
  testthat::expect_equal(get_sd_from_cv(1.0), sqrt(log(1.0^2 + 1)), tolerance = 1e-6)

  # Test with extreme value
  testthat::expect_equal(get_sd_from_cv(0), 0, tolerance = 1e-6)
})

testthat::test_that("get_cv_from_sd function correctly converts SD to CV", {
  # Test with known values
  testthat::expect_equal(get_cv_from_sd(0.1), sqrt(exp(0.1^2) - 1), tolerance = 1e-6)
  testthat::expect_equal(get_cv_from_sd(0.5), sqrt(exp(0.5^2) - 1), tolerance = 1e-6)
  testthat::expect_equal(get_cv_from_sd(1.0), sqrt(exp(1.0^2) - 1), tolerance = 1e-6)

  # Test with extreme value
  testthat::expect_equal(get_cv_from_sd(0), 0, tolerance = 1e-6)
})

testthat::test_that("get_sd_from_cv and get_cv_from_sd are inverse functions", {
  # Test that they are inverse functions
  cv_values <- c(0.1, 0.5, 1.0, 2.0)
  for (cv in cv_values) {
    sd <- get_sd_from_cv(cv)
    cv_back <- get_cv_from_sd(sd)
    testthat::expect_equal(cv_back, cv, tolerance = 1e-6)
  }

  # Test the other direction
  sd_values <- c(0.1, 0.5, 1.0, 2.0)
  for (sd in sd_values) {
    cv <- get_cv_from_sd(sd)
    sd_back <- get_sd_from_cv(cv)
    testthat::expect_equal(sd_back, sd, tolerance = 1e-6)
  }
})
