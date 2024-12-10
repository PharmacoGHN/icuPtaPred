
# Define the test cases
testthat::test_that("Correct calculations for CG formula", {
   # test with date calculation
   age_calc <- round(as.numeric(difftime(as.Date("2023/08/19"), as.Date("1983/08/19"), units = "days") / 365.25), digits = 0)

   # Test with creat_unit = "uM/L", ethnicity = "African"
   testthat::expect_equal(renal_function(sex = "Male", age = 40, weight = 70, creat = 81.2, formula = "CG", creat_unit = "uM/L", ethnicity = "African"), 106)
   testthat::expect_equal(renal_function(sex = "Female", age = 55, weight = 65, creat = 106.7, formula = "CG", creat_unit = "uM/L", ethnicity = "African"), 53.9)
   testthat::expect_equal(renal_function(sex = "Male", age = age_calc, weight = 70, creat = 81.2, formula = "CG", creat_unit = "uM/L", ethnicity = "African"), 106)
   # Test with creat_unit = "mg/dL", ethnicity = "African"
   testthat::expect_equal(renal_function(sex = "Male", age = 40, weight = 70, creat = 0.9, formula = "CG", creat_unit = "mg/dL", ethnicity = "African"), 108.2)
   testthat::expect_equal(renal_function(sex = "Female", age = 55, weight = 65, creat = 1.13, formula = "CG", creat_unit = "mg/dL", ethnicity = "African"), 57.5)
   # Test with creat_unit = "uM/L", ethnicity = "Non-African"
   testthat::expect_equal(renal_function(sex = "Male", age = 40, weight = 70, creat = 81.2, formula = "CG", creat_unit = "uM/L", ethnicity = "Non-African"), 106.0)
   testthat::expect_equal(renal_function(sex = "Female", age = 55, weight = 65, creat = 106.7, formula = "CG", creat_unit = "uM/L", ethnicity = "Non-African"), 53.9)
   # Test with creat_unit = "mg/dL", ethnicity = "Non-African"
   testthat::expect_equal(renal_function(sex = "Male", age = 40, weight = 70, creat = 0.9, formula = "CG", creat_unit = "mg/dL", ethnicity = "Non-African"), 108.2)
   testthat::expect_equal(renal_function(sex = "Female", age = 55, weight = 65, creat = 1.13, formula = "CG", creat_unit = "mg/dL", ethnicity = "Non-African"), 57.5)
})

testthat::test_that("Correct calculations for MDRD formula", {
   # Test with creat_unit = "uM/L", ethnicity = "African"
   testthat::expect_equal(renal_function(sex = "Male", age = 60, weight = 80, creat = 81.2, formula = "MDRD", creat_unit = "uM/L", ethnicity = "African"), 62.4)
   testthat::expect_equal(renal_function(sex = "Female", age = 70, weight = 60, creat = 106.7, formula = "MDRD", creat_unit = "uM/L", ethnicity = "African"), 53.5)
   # Test with creat_unit = "mg/dL", ethnicity = "African"
   testthat::expect_equal(renal_function(sex = "Male", age = 60, weight = 80, creat = 0.9, formula = "MDRD", creat_unit = "mg/dL", ethnicity = "African"), 63.9)
   testthat::expect_equal(renal_function(sex = "Female", age = 70, weight = 60, creat = 1.13, formula = "MDRD", creat_unit = "mg/dL", ethnicity = "African"), 57.7)
   # Test with creat_unit = "uM/L", ethnicity = "Non-African"
   testthat::expect_equal(renal_function(sex = "Male", age = 60, weight = 80, creat = 81.2, formula = "MDRD", creat_unit = "uM/L", ethnicity = "Non-African"), 84.1)
   testthat::expect_equal(renal_function(sex = "Female", age = 70, weight = 60, creat = 106.7, formula = "MDRD", creat_unit = "uM/L", ethnicity = "Non-African"), 72.1)
   # Test with creat_unit = "mg/dL", ethnicity = "Non-African"
   testthat::expect_equal(renal_function(sex = "Male", age = 60, weight = 80, creat = 0.9, formula = "MDRD", creat_unit = "mg/dL", ethnicity = "Non-African"), 86.1)
   testthat::expect_equal(renal_function(sex = "Female", age = 70, weight = 60, creat = 1.13, formula = "MDRD", creat_unit = "mg/dL", ethnicity = "Non-African"), 77.8)
})

testthat::test_that("Correct calculations for CKD_2009 formula", {
   # Test with creat_unit = "uM/L", ethnicity = "African"
   testthat::expect_equal(renal_function(sex = "Male", age = 50, weight = 75, creat = 81.2, formula = "CKD_2009", creat_unit = "uM/L", ethnicity = "African"), 112.2)
   testthat::expect_equal(renal_function(sex = "Female", age = 45, weight = 70, creat = 106.7, formula = "CKD_2009", creat_unit = "uM/L", ethnicity = "African"), 62.8)
   # Test with creat_unit = "mg/dL", ethnicity = "African"
   testthat::expect_equal(renal_function(sex = "Male", age = 50, weight = 75, creat = 0.9, formula = "CKD_2009", creat_unit = "mg/dL", ethnicity = "African"), 115)
   testthat::expect_equal(renal_function(sex = "Female", age = 45, weight = 70, creat = 1.13, formula = "CKD_2009", creat_unit = "mg/dL", ethnicity = "African"), 68)
   # Test with creat_unit = "uM/L", ethnicity = "Non-African"
   testthat::expect_equal(renal_function(sex = "Male", age = 50, weight = 75, creat = 81.2, formula = "CKD_2009", creat_unit = "uM/L", ethnicity = "Non-African"), 96.8)
   testthat::expect_equal(renal_function(sex = "Female", age = 45, weight = 70, creat = 106.7, formula = "CKD_2009", creat_unit = "uM/L", ethnicity = "Non-African"), 54.2)
   # Test with creat_unit = "mg/dL", ethnicity = "Non-African"
   testthat::expect_equal(renal_function(sex = "Male", age = 50, weight = 75, creat = 0.9, formula = "CKD_2009", creat_unit = "mg/dL", ethnicity = "Non-African"), 99.2)
   testthat::expect_equal(renal_function(sex = "Female", age = 45, weight = 70, creat = 1.13, formula = "CKD_2009", creat_unit = "mg/dL", ethnicity = "Non-African"), 58.6)
})

testthat::test_that("Correct calculations for CKD_2021 formula", {
   # Test with creat_unit = "uM/L", ethnicity = "African"
   testthat::expect_equal(renal_function(sex = "Male", age = 65, weight = 90, creat = 81.2, formula = "CKD_2021", creat_unit = "uM/L", ethnicity = "African"), 87.8)
   testthat::expect_equal(renal_function(sex = "Female", age = 55, weight = 68, creat = 106.7, formula = "CKD_2021", creat_unit = "uM/L", ethnicity = "African"), 51.1)
   # Test with creat_unit = "mg/dL", ethnicity = "African"
   testthat::expect_equal(renal_function(sex = "Male", age = 65, weight = 90, creat = 0.9, formula = "CKD_2021", creat_unit = "mg/dL", ethnicity = "African"), 89.9)
   testthat::expect_equal(renal_function(sex = "Female", age = 55, weight = 68, creat = 1.13, formula = "CKD_2021", creat_unit = "mg/dL", ethnicity = "African"), 55.3)
   # Test with creat_unit = "uM/L", ethnicity = "Non-African"
   testthat::expect_equal(renal_function(sex = "Male", age = 65, weight = 90, creat = 81.2, formula = "CKD_2021", creat_unit = "uM/L", ethnicity = "Non-African"), 87.8)
   testthat::expect_equal(renal_function(sex = "Female", age = 55, weight = 68, creat = 106.7, formula = "CKD_2021", creat_unit = "uM/L", ethnicity = "Non-African"), 51.1)
   # Test with creat_unit = "mg/dL", ethnicity = "Non-African"
   testthat::expect_equal(renal_function(sex = "Male", age = 65, weight = 90, creat = 0.9, formula = "CKD_2021", creat_unit = "mg/dL", ethnicity = "Non-African"), 89.9)
   testthat::expect_equal(renal_function(sex = "Female", age = 55, weight = 68, creat = 1.13, formula = "CKD_2021", creat_unit = "mg/dL", ethnicity = "Non-African"), 55.3)
})

testthat::test_that("Correct calculations for UV/P formula", {
   # Test with creat_unit = "uM/L"
   testthat::expect_equal(renal_function(sex = "Male", age = 65, weight = 90, creat = 40, formula = "UVP", creat_unit = "uM/L", urine_creat = 36, urine_output = 100), 62.5)
   testthat::expect_equal(renal_function(sex = "Male", age = 65, weight = 90, creat = 40, formula = "UVP", creat_unit = "uM/L", urine_creat = 4.5, urine_output = 1000), 78.1)

   # Test with creat_unit = "mg/dL", ethnicity = "African"
   testthat::expect_equal(renal_function(sex = "Male", age = 65, weight = 90, creat = 0.9,  formula = "UVP", creat_unit = "mg/dL", urine_creat = 1.8, urine_output = 100), 13.9)
   testthat::expect_equal(renal_function(sex = "Male", age = 55, weight = 68, creat = 1.13, formula = "UVP", creat_unit = "mg/dL", urine_creat = 2, urine_output = 1000), 122.9)

})

testthat::test_that("Correct calculations for Schwartz formula", {
   # Test with creat_unit = "uM/L"
   testthat::expect_equal(renal_function(sex = "Male", age = 2, height = 90, creat = 40, formula = "schwartz", creat_unit = "uM/L"), 109.4)
   testthat::expect_equal(renal_function(sex = "Male", age = 13, height = 90, creat = 40, formula = "schwartz", creat_unit = "uM/L"), 139.2)

   # Test with creat_unit = "mg/dL", ethnicity = "African"
   testthat::expect_equal(renal_function(sex = "Male", age = 2, height = 90, creat = 0.9,  formula = "schwartz", creat_unit = "mg/dL"), 55)
   testthat::expect_equal(renal_function(sex = "Male", age = 13, height = 68, creat = 1.13, formula = "schwartz", creat_unit = "mg/dL"), 42.1)

})

testthat::test_that("Correct calculations 'none' formula", {
   # Test with creat_unit = "uM/L"
   testthat::expect_equal(renal_function(sex = "Male", age = 65, weight = 90, creat = 40, formula = "none", creat_unit = "uM/L"), 1)
})