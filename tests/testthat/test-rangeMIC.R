
# Test for default behavior
test_that("Default behavior returns correct MIC range", {
  expect_equal(rangeMIC(), c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64))
})

# Test for custom min, max, and step values
test_that("Custom min, max, and step return correct MIC range", {
  expect_equal(rangeMIC(min = 0.5, max = 8, step = 2), c(0.5, 1, 2, 4, 8))
})

# Test for continuous range
test_that("Continuous mode returns correct MIC range", {
  result <- rangeMIC(min = 0.5, max = 1, continuous = TRUE)
  expect_equal(result, seq(0.5, 1, by = 0.1))
})

# Test for invalid input: min greater than max
# test_that("Min greater than max throws an error or returns empty", {
# expect_error(rangeMIC(min = 64, max = 0.0625), "error")
# })

# Test for custom drug names (ensure it doesn’t affect computation)
test_that("Drug parameter does not affect MIC range", {
  expect_equal(rangeMIC(drug = c("amoxicillin", "clavulanic_acid")),
               c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64))
})

# Test for a large step size
# test_that("Large step size returns correct MIC range", {
#   expect_equal(rangeMIC(min = 1, max = 1000, step = 10), c(1, 10, 100, 1000))
# })

# Test for single-step range (min equals max)
test_that("Single-step range returns correct MIC range", {
  expect_equal(rangeMIC(min = 1, max = 1), c(1))
})

# Test for non-integer step values
test_that("Non-integer step returns correct MIC range", {
  expect_equal(rangeMIC(min = 0.5, max = 4, step = 1.5), c(0.5, 0.75, 1.125, 1.6875, 2.53125, 3.796875))
})
