box::use(
  testthat[expect_equal, test_that]
)

box::use(
  app/logic/pta_service[build_plot_mic_grid]
)

test_that("build_plot_mic_grid fills sparse MIC gaps for plotting", {
  observed_mic <- c(0.008, 0.06, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512)

  plot_grid <- build_plot_mic_grid(observed_mic)

  expect_equal(
    plot_grid,
    c(0.008, 0.016, 0.03, 0.06, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512)
  )
})