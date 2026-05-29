box::use(
  testthat[expect_true, expect_type, test_that]
)

box::use(
  app/main
)

test_that("main module exports ui and server", {
  expect_type(main$ui, "closure")
  expect_type(main$server, "closure")
  expect_true("id" %in% names(formals(main$ui)))
  expect_true("id" %in% names(formals(main$server)))
})