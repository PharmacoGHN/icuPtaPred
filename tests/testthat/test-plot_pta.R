box::use(
  testthat[expect_equal, expect_no_error, expect_s3_class, expect_true, test_that]
)

box::use(
  app/view/plot_pta
)

impl <- attr(plot_pta, "namespace")

test_that("format_plot_value handles vector inputs", {
  expect_equal(
    impl$format_plot_value(c(1, Inf, NA_real_, 12.3456789)),
    c("1", "NA", "NA", "12.3457")
  )
})

test_that("safe_log_limits preserves MIC values below 0.01", {
  expect_equal(impl$safe_log_limits(c(0.008, 0.06))[1], 0.008)
})

test_that("plot.pta returns the three PTA views", {
  data <- data.frame(
    mic = c(0.5, 1, 2),
    css_mic = c(2, 1, 0.5),
    css_mic_below1 = c(1.6, 0.8, 0.4),
    css_mic_below2 = c(1.2, 0.6, 0.3),
    css_mic_above1 = c(2.4, 1.2, 0.6),
    css_mic_above2 = c(2.8, 1.4, 0.7),
    percentile_2.5 = c(1.7, 0.9, 0.45),
    percentile_97.5 = c(2.3, 1.1, 0.55),
    toxicity_threshold = c(4, 4, 4),
    additional_threshold = c(NA_real_, 1.5, NA_real_)
  )

  plots <- NULL

  expect_no_error({
    plots <- plot_pta$plot.pta(data, ecoff = 1, selected_dose = 4, dose_increment = 1)
  })

  expect_s3_class(plots$pta_plot, "ggplot")
  expect_s3_class(plots$pta_multiple_doses, "ggplot")
  expect_s3_class(plots$pta_ci_plot, "ggplot")
  expect_equal(plots$pta_plot$labels$y, "Css/MIC ratio")

  ci_layer <- plots$pta_ci_plot$layers[[length(plots$pta_ci_plot$layers)]]
  ci_colour <- ci_layer$aes_params$colour

  if (is.null(ci_colour)) {
    ci_colour <- ci_layer$aes_params$col
  }

  expect_equal(ci_colour, NA)
})

test_that("plot.pta labels the axis and hover text for free Css", {
  data <- data.frame(
    mic = c(0.5, 1, 2),
    css_mic = c(2, 1, 0.5),
    css_mic_below1 = c(1.6, 0.8, 0.4),
    css_mic_below2 = c(1.2, 0.6, 0.3),
    css_mic_above1 = c(2.4, 1.2, 0.6),
    css_mic_above2 = c(2.8, 1.4, 0.7),
    percentile_2.5 = c(1.7, 0.9, 0.45),
    percentile_97.5 = c(2.3, 1.1, 0.55),
    toxicity_threshold = c(4, 4, 4),
    additional_threshold = c(NA_real_, 1.5, NA_real_)
  )

  plots <- plot_pta$plot.pta(data, ecoff = 1, selected_dose = 4, dose_increment = 1, use_free_fraction = TRUE)
  plot_data <- impl$build_pta_plot_data(data, selected_dose = 4, dose_increment = 1, use_free_fraction = TRUE)

  expect_equal(plots$pta_plot$labels$y, "Free Css/MIC ratio")
  expect_true(grepl("free Css/MIC ratio", plot_data$selected_hover[[1]], fixed = TRUE))
})

test_that("plot.pta builds a single toxicity marker at MIC 1", {
  data <- data.frame(
    mic = c(0.25, 0.5, 2),
    css_mic = c(4, 2, 0.5),
    css_mic_below1 = c(3.2, 1.6, 0.4),
    css_mic_below2 = c(2.4, 1.2, 0.3),
    css_mic_above1 = c(4.8, 2.4, 0.6),
    css_mic_above2 = c(5.6, 2.8, 0.7),
    percentile_2.5 = c(3.4, 1.7, 0.45),
    percentile_97.5 = c(4.6, 2.3, 0.55),
    toxicity_threshold = c(4, 4, 4),
    additional_threshold = c(NA_real_, 1.5, NA_real_)
  )

  toxicity_marker <- impl$build_toxicity_marker_data(data)

  expect_equal(toxicity_marker$mic[[1]], 1)
  expect_equal(toxicity_marker$toxicity_threshold[[1]], 4)
})

test_that("plot.cfr builds hover text for all rows", {
  data <- data.frame(
    dose = c(0, 2000, 4000),
    cfr = c(0, 0.4, 0.9),
    toxicity_proportion = c(NA_real_, 0.05, 0.1)
  )

  plot <- NULL

  expect_no_error({
    plot <- plot_pta$plot.cfr(data)
  })

  expect_s3_class(plot, "ggplot")
})