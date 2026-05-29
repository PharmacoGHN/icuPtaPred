box::use(
  testthat[expect_equal, test_that]
)

box::use(
  app/logic/fct_weight_formula[weight_formula]
)

test_that("weight_formula return the right output", {
  # test unit = kg
  expect_equal(weight_formula(weight = 70, height = 170, sex = "Female", weight_unit = "kg", formula = "IBW"), 61.4)
  expect_equal(weight_formula(weight = 70, height = 170, sex = "Female", weight_unit = "kg", formula = "AJBW"), 64.9)
  expect_equal(weight_formula(weight = 70, height = 170, sex = "Female", weight_unit = "kg", formula = "LBW"), 44.2)


  # test unit = lbs
  expect_equal(weight_formula(weight = 154.324, height = 170, sex = "Female", weight_unit = "lbs", formula = "IBW"), 61.4)
  expect_equal(weight_formula(weight = 154.324, height = 170, sex = "Female", weight_unit = "lbs", formula = "AJBW"), 64.9)
  expect_equal(weight_formula(weight = 154.324, height = 170, sex = "Female", weight_unit = "lbs", formula = "LBW"), 44.2)

  # test sex = Male
  expect_equal(weight_formula(weight = 70, height = 170, sex = "Male", weight_unit = "kg", formula = "IBW"), 65.9)
  expect_equal(weight_formula(weight = 70, height = 170, sex = "Male", weight_unit = "kg", formula = "AJBW"), 67.6)
  expect_equal(weight_formula(weight = 70, height = 170, sex = "Male", weight_unit = "kg", formula = "LBW"), 54.5)
})