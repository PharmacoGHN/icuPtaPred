box::use(
  testthat[expect_equal, expect_false, expect_s3_class, expect_true, test_that]
)

box::use(
  app/logic/pta_service
)

impl <- attr(pta_service, "namespace")

test_that("build_plot_mic_grid fills sparse MIC gaps for plotting", {
  observed_mic <- c(0.008, 0.06, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512)

  plot_grid <- pta_service$build_plot_mic_grid(observed_mic)

  expect_equal(
    plot_grid,
    c(0.008, 0.016, 0.03, 0.06, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512)
  )
})

test_that("calc_css_distribution returns a matrix for multiple doses", {
  css_distribution <- impl$calc_css_distribution(
    dose = c(1000, 2000, 3000),
    tvcl = 10,
    eta_cl = 0.2,
    n_sim = 25
  )

  expect_equal(dim(css_distribution), c(25, 3))
  expect_true(all(css_distribution[, 2] > css_distribution[, 1]))
  expect_true(all(css_distribution[, 3] > css_distribution[, 2]))
})

test_that("sim_concentration returns one row per MIC and additional threshold column", {
  result <- pta_service$sim_concentration(
    dose = 1000,
    tvcl = 10,
    eta_cl = 0.2,
    quantile = c(0.025, 0.975),
    mic = c(0.125, 0.25, 0.5, 1),
    dose_increment = 500,
    toxicity_threshold = 8,
    additional_threshold = 12,
    n_sim = 250
  )

  expect_s3_class(result, "data.frame")
  expect_equal(
    colnames(result),
    c(
      "css_mic",
      "mic",
      "percentile_2.5",
      "percentile_97.5",
      "css_mic_below2",
      "css_mic_below1",
      "css_mic_above1",
      "css_mic_above2",
      "toxicity_threshold",
      "additional_threshold"
    )
  )
  expect_equal(nrow(result), 4)
  expect_true(all(diff(result$css_mic) < 0))
  expect_false(any(is.na(result$additional_threshold)))
})

test_that("sim_concentration applies the free-fraction multiplier to Css-derived series", {
  result <- pta_service$sim_concentration(
    dose = 240,
    tvcl = 10,
    eta_cl = 0,
    quantile = c(0.025, 0.975),
    css_quantile = 0.95,
    mic = c(0.5, 1),
    dose_increment = 120,
    toxicity_threshold = 4,
    additional_threshold = 6,
    n_sim = 1,
    concentration_multiplier = 0.5
  )

  expect_equal(result$css_mic, c(1, 0.5))
  expect_equal(result$toxicity_threshold, c(4, 2))
  expect_equal(result$additional_threshold, c(6, 3))
})

test_that("calculate_cfr_mulitple_doses reports CFR and toxicity across a dose range", {
  result <- pta_service$calculate_cfr_mulitple_doses(
    dose_increment = 1000,
    dose_max = 3000,
    tvcl = 10,
    eta_cl = 0.2,
    mic_distribution = data.frame(
      mic = c(0.5, 1, 2),
      distribution = c(20, 30, 50)
    ),
    toxicity_threshold = 6,
    n_sim = 250
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true(all(result$cfr >= 0 & result$cfr <= 1))
  expect_true(all(result$toxicity_proportion >= 0 & result$toxicity_proportion <= 1))
})