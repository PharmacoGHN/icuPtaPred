box::use(
   testthat[expect_length, expect_type, skip_if, test_that]
)

box::use(
   app/logic/fct_extract_eucast[mic_distribution, update_eucast]
)

test_that("extract_eucast return the right list and is up to date", {

   res_update_eucast <- update_eucast()
   res_mic_distribution <- mic_distribution("Vancomycin", "Staphylococcus lugdunensis", res_update_eucast)


   # tests update eucast is up to date and return the right output
    expect_type(res_update_eucast, "list")
    expect_length(res_update_eucast, 2)
    expect_type(res_mic_distribution, "list")
    expect_length(res_mic_distribution, 4)
    expect_length(res_mic_distribution[[1]][1, ], 24) # 1 df with 24 column (mic and ECOFF) (2023/10/05)
    expect_length(res_mic_distribution[[2]], 4)
    expect_length(res_mic_distribution[[3]], 1)

    skip_if(length(res_update_eucast[[1]][, 1]) != 187, message = "The list of antibiotics in eucast was updated since the 2025/03/04")
    expect_length(res_update_eucast[[1]][, 1], 187) # 187 antibiotics and their rank (2025/03/04)

    skip_if(length(res_update_eucast[[2]][, 1]) != 480, message = "The list of bacteria in eucast was updated since the 2025/03/04")
    expect_length(res_update_eucast[[2]][, 1], 480) # 480 bacteria and their rank (2025/03/04)

})
