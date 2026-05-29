box::use(
   testthat[expect_equal, expect_length, expect_true, expect_type, test_that]
)

box::use(
   app/logic/fct_extract_eucast[mic_distribution, read_eucast_mic, update_eucast]
)

test_that("update_eucast reads the bundled lookup tables", {
  res_update_eucast <- update_eucast("app/static/eucast.json")

  expect_type(res_update_eucast, "list")
  expect_length(res_update_eucast, 2)
  expect_equal(colnames(res_update_eucast[[1]]), c("antibiotics", "atb_values"))
  expect_equal(colnames(res_update_eucast[[2]]), c("bacteria", "bacteria_values"))
  expect_true(nrow(res_update_eucast[[1]]) > 0)
  expect_true(nrow(res_update_eucast[[2]]) > 0)
})

test_that("mic_distribution reads a cached MIC entry from JSON", {
  eucast_mic <- read_eucast_mic("tests/testthat/fixtures/eucast_mic.json")
  res_mic_distribution <- mic_distribution("Vancomycin", "Staphylococcus lugdunensis", eucast_mic)

  expect_type(res_mic_distribution, "list")
  expect_length(res_mic_distribution, 3)
  expect_equal(colnames(res_mic_distribution$mic_distribution), c("mic", "distribution"))
  expect_equal(nrow(res_mic_distribution$mic_distribution), 4)
  expect_equal(res_mic_distribution$ecoff, "2")
  expect_equal(res_mic_distribution$ecoff_ci, "1 - 2")
})
