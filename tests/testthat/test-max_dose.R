
test_that("get_model_parameters returns correct toxicity_threshold values", {
  expect_equal(max_dose("Amoxicillin"), 20)
  expect_equal(max_dose("Cefepime"), 20)
  expect_equal(max_dose("Cefazoline"), 20)
  expect_equal(max_dose("Cefotaxim"), 20)
  expect_equal(max_dose("Cefiderocol"), 20)
  expect_equal(max_dose("Ceftazidime"), 20)
  expect_equal(max_dose("Ceftaroline"), 20)
  expect_equal(max_dose("Ceftobiprol"), 20)
  expect_equal(max_dose("Ceftolozane"), 20)
  expect_equal(max_dose("Piperacillin-tazobactam"), 40)
  expect_equal(max_dose("Meropenem"), 20)
})