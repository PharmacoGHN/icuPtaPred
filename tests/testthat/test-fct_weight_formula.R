testthat::test_that("weight_formula return the right output", {
  # test unit = kg
  testthat::expect_equal(weight_formula(weight = 70, height = 170, sex = "Female", weight_unit = "kg", formula = "IBW"), 61.4)
  testthat::expect_equal(weight_formula(weight = 70, height = 170, sex = "Female", weight_unit = "kg", formula = "AJBW"), 64.9)
  testthat::expect_equal(weight_formula(weight = 70, height = 170, sex = "Female", weight_unit = "kg", formula = "LBW"), 44.2)


  # test unit = lbs
  testthat::expect_equal(weight_formula(weight = 31.758, height = 170, sex = "Female", weight_unit = "lbs", formula = "IBW"), 61.4)
  testthat::expect_equal(weight_formula(weight = 31.75, height = 170, sex = "Female", weight_unit = "lbs", formula = "AJBW"), 64.9)
  testthat::expect_equal(weight_formula(weight = 31.75, height = 170, sex = "Female", weight_unit = "lbs", formula = "LBW"), 44.2)

  # test sex = Male
  testthat::expect_equal(weight_formula(weight = 70, height = 170, sex = "Male", weight_unit = "kg", formula = "IBW"), 65.9)
  testthat::expect_equal(weight_formula(weight = 70, height = 170, sex = "Male", weight_unit = "kg", formula = "AJBW"), 67.6)
  testthat::expect_equal(weight_formula(weight = 70, height = 170, sex = "Male", weight_unit = "kg", formula = "LBW"), 54.5)
})