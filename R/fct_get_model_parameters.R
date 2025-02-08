#' get_model_parameters
#'
#' @description this function calculate pk parameters and get the dose increment base on model
#' and drug chosen.
#'
#' @return return drug clearance, clearance IIV and step increment based on chosen drug
#'
#' @noRd

get_model_parameters <- function(model, biological, drug = NULL) {

  cl <- dplyr::case_when(
    # Piperacillin
    model == "klastrup_2020" ~ (2.25 + 0.119 * biological$cg_tbw),

    # Cefepim
    model == "an_2023" ~ 4, #(2 * biological$cg_lbw / 54 + 0.526), # simplified only patient without CRRT
    model == "cacqueray_2022" ~ 1.21 * (biological$tbw / 9) ^ 0.75 * (biological$schwartz / 153) ^ 0.37,
    model == "guohua_2023" ~ 0.526 + 2 * biological$cg_lbw / 54,

    # default value is set to 0 if the drug hasn't been implemented yet
    .default = 0
  )

  eta_cl <- dplyr::case_when(

    # Amoxicillin eta CL ___

    # Piperacillin model ____
    model == "klastrup_2020" ~ 0.533,

    # Cefepim eta CL ___
    model == "an_2023" ~ 1, # get the right value
    model == "caqueray_2022" ~ 0.39,
    model == "guohua_2023" ~ 0.293,

    # default value is set to 0 if the drug hasn't been implemented yet
    .default = 0
  )

  # automatic selection of dose increment based on selected drug.
  # This dose increment is in gram
  dose_increment <- dplyr::case_when(
    drug == "amoxicillin" ~ 500,
    drug == "cefepim" ~ 1000,
    drug == "cefazoline" ~ 500,
    drug == "cefotaxim" ~ 500,
    drug == "ceftazidim" ~ 1000,
    drug == "ceftaroline" ~ 1000,
    drug == "ceftobiprol" ~ 1000,
    drug == "pipetazo" ~ 2000,
    drug == "meropenem" ~ 500,
    .default = 0
  )
  return(list(cl = cl, eta_cl = eta_cl, dose_increment = dose_increment))
}
