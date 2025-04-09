testthat::test_that("extract_eucast return the right list and is up to date", {

   res_update_eucast <- update_eucast()
   res_mic_distribution <- mic_distribution("Vancomycin", "Staphylococcus lugdunensis", res_update_eucast)


   # tests update eucast is up to date and return the right output
   testthat::expect_type(res_update_eucast, "list")
   testthat::expect_length(res_update_eucast, 2)
   testthat::expect_type(res_mic_distribution, "list")
   testthat::expect_length(res_mic_distribution, 4)
   testthat::expect_length(res_mic_distribution[[1]][1, ], 24) # 1 df with 24 column (mic and ECOFF) (2023/10/05)
   testthat::expect_length(res_mic_distribution[[2]], 4)
   testthat::expect_length(res_mic_distribution[[3]], 1)

   testthat::skip_if(length(res_update_eucast[[1]][, 1]) != 187, message = "The list of antibiotics in eucast was updated since the 2025/03/04")
   testthat::expect_length(res_update_eucast[[1]][, 1], 187) # 187 antibiotics and their rank (2025/03/04)

   testthat::skip_if(length(res_update_eucast[[2]][, 1]) != 480, message = "The list of bacteria in eucast was updated since the 2025/03/04")
   testthat::expect_length(res_update_eucast[[2]][, 1], 480) # 480 bacteria and their rank (2025/03/04)

})
