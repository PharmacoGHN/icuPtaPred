box::use(
   testthat[expect_equal, expect_error, test_that]
)

box::use(
   app/logic/fct_bsa[bsa]
)

test_that("bsa output are correct", {

    expect_error(bsa(170, 60, capped = 10))
    expect_equal(bsa(height = 170, weight = 60), 1.69)
    expect_equal(bsa(height = 170, weight = 100, capped = TRUE), 2)
    expect_equal(bsa(height = 170, weight = 100), 2.11)
})