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
