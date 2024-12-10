testthat::test_that("bsa output are correct", {

   testthat::expect_error(bsa(170, 60, capped = 10))
   testthat::expect_equal(bsa(height = 170, weight = 60), 1.69)
   testthat::expect_equal(bsa(height = 170, weight = 100, capped = TRUE), 2)
   testthat::expect_equal(bsa(height = 170, weight = 100), 2.11)
})