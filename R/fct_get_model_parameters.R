#' get_model_parameters
#'
#' @description this function calculate pk parameters and get the dose increment base on model
#' and drug chosen.
#'
#' @return return drug clearance, clearance IIV and step increment based on chosen drug
#'
#' @noRd

get_model_parameters <- function(model, biological, drug = NULL) {
  cl <- switch(model,
    # Cefepim
    "an_2023" = 4, # (2 * biological$cg_lbw / 54 + 0.526), # simplified only patient without CRRT
    "cacqueray_2022" = 1.21 * (biological$tbw / 9)^0.75 * (biological$schwartz / 153)^0.37,
    "guohua_2023" = 0.526 + 2 * biological$cg_lbw / 54,

    # Ceftazidime
    "Buning_2021" = 1,
    "Launay_2024" = 1,
    "Cojutti_2024" = 1,

    # Ceftolozane
    "Chandorkar_2015" = 1,
    "Zhang_2021" = 1,

    # Meropenem
    "Gijsen_2021" = 1,
    "Minichmayr_2024" = 1,
    "Ehrmann_2019" = 1,
    "Huang_2025" = 1,
    "Lan_2022" = 1,
    "Fukumoto_2023" = 1.35 * ((biological$uvp * 1.73 / biological$bsa) / 87.6)^0.67,

    # Piperacillin
    "klastrup_2020" = (2.25 + 0.119 * biological$cg_tbw),
    "Sukarnjanaset_2019" = 0,
    "Udy_2015" = 1,
    # default value if no match
    1
  )

  eta_cl <- switch(model,
    # Cefepim eta CL
    "an_2023" = get_sd_from_cv(0.299),
    "cacqueray_2022" = 0.39,
    "guohua_2023" = 0.293,

    # Ceftazidime
    "Buning_2021" = 1,
    "Launay_2024" = 1,
    "Cojutti_2024" = 1,

    # Ceftolozane
    "Chandorkar_2015" = 1,
    "Zhang_2021" = 1,

    # Meropenem
    "Gijsen_2021" = 1,
    "Minichmayr_2024" = 1,
    "Ehrmann_2019" = 1,
    "Huang_2025" = 1,
    "Lan_2022" = 1,
    "Fukumoto_2023" = get_sd_from_cv(0.221),

    # Piperacillin
    "klastrup_2020" = 0.533,
    "Sukarnjanaset_2019" = 1,
    "Udy_2015" = 1,

    # default value if no match
    1
  )

  # automatic selection of dose increment based on selected drug.
  # This dose increment is in gram
  dose_increment <- dplyr::case_when(
    drug == "amoxicillin" ~ 0.500,
    drug == "cefepim" ~ 1.000,
    drug == "cefazoline" ~ 0.500,
    drug == "cefotaxim" ~ 0.500,
    drug == "ceftazidim" ~ 1.000,
    drug == "ceftaroline" ~ 1.000,
    drug == "ceftobiprol" ~ 1.000,
    drug == "pipetazo" ~ 2.000,
    drug == "meropenem" ~ 0.500,
    .default = 0
  )
  return(list(cl = cl, eta_cl = eta_cl, dose_increment = dose_increment))
}



drug_threshold <- function(drug) {
  toxicity <- switch(drug,
    "amoxicillin" = NA,
    "cefepim" = 20, # Lamoth et al. 2010
    "cefazoline" = NA,
    "cefotaxim" = NA,
    "ceftazidim" = NA,
    "ceftaroline" = NA,
    "ceftobiprol" = NA,
    "pipetazo" = 157,
    "meropenem" = 45, # Scharf C et al, 2020 https://pmc.ncbi.nlm.nih.gov/articles/PMC7148485/
    0
  )

  return(toxicity)
}
