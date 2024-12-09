testthat::test_that("extract_eucast return the right list and is up to date", {

   res_update_eucast <- update_eucast()
   res_mic_distribution <- mic_distribution("Vancomycin", "Staphylococcus lugdunensis", res_update_eucast)


   # tests update eucast is up to date and return the right output
   testthat::expect_type(res_update_eucast, "list")
   testthat::expect_length(res_update_eucast, 2)
   testthat::expect_type(res_mic_distribution, "list")
   testthat::expect_length(res_mic_distribution, 3)
   testthat::expect_length(res_mic_distribution[[1]][1, ], 24) # 1 df with 24 column (mic and ECOFF) (2023/10/05)
   testthat::expect_length(res_mic_distribution[[2]], 1)
   testthat::expect_length(res_mic_distribution[[3]], 1)

   testthat::skip_if(length(res_update_eucast[[1]][, 1]) != 178, message = "The list of antibiotics in eucast was updated since the 2023/10/05")
   testthat::expect_length(res_update_eucast[[1]][, 1], 178) # 178 antibiotics and their rank (2023/10/05)

   testthat::skip_if(length(res_update_eucast[[2]][, 1]) != 486, message = "The list of bacteria in eucast was updated since the 2023/10/05")
   testthat::expect_length(res_update_eucast[[2]][, 1], 486) # 486 bacteria and their rank (2023/10/05)

})