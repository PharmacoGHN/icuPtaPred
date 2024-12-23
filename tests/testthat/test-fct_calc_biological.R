test_that("calc_biological calculates biological indices correctly", {
  sex <- "Male"
  age <- 30
  weight <- 70
  height <- 170
  creatinine <- 70
  weight_unit <- "kg"
  creat_unit <- "uM/L"

  result <- calc_biological(sex, age, weight, height, creatinine, weight_unit, creat_unit)

  # Test body weights
  expect_equal(result$tbw, 70)
  expect_equal(result$lbw, 54.5)
  expect_equal(result$ajbw, 67.6)
  expect_equal(result$ibw, 65.9)

  # Test BMI
  expect_equal(result$bmi, round(weight / (height / 100)^2, digits = 1))

  # Test BSA
  expect_equal(result$bsa, 1.81)

  # Test renal functions
  expect_equal(result$cg_tbw, 135.3)
  expect_equal(result$cg_ajbw, 130.7)
  expect_equal(result$cg_ibw, 127.4)
  expect_equal(result$cg_lbw, 105.3)

  expect_equal(result$mdrd, 114.9)
  expect_equal(result$ckd_2009, 120.4)
  expect_equal(result$ckd_2021, 119.6)
  expect_equal(result$schwartz, 150.3)
})