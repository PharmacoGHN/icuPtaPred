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
    "Barreto_2023" = 7.84, #eGFR cyst-creat not supported at the moment.
    "cacqueray_2022" = 1.21 * (biological$tbw / 9)^0.75 * (biological$schwartz / 153)^0.37,
    "an_2023" = 0.526 + 2 * biological$cg_lbw / 54,

    # Ceftazidime
    "Buning_2021" = 3.42 * (biological$ckd_2009 / 73)^0.772, # TODO add trauma and hematology malignancy
    "Launay_2024" = 4.45 * (biological$ckd_2009 / 73.9)^0.9,
    "Cojutti_2024" = 5 * (biological$ekfc / 70) ^ 0.7,

    # Ceftolozane
    "Chandorkar_2015" = 5.11 * 1.215 * (biological$cg_tbw / 109) ^ 0.715,
    "Zhang_2021" = 4.84 * (biological$cg_tbw / 100) ^ 0.701,

    # Meropenem
    "Gijsen_2021" = 1,
    "Minichmayr_2018" = 1,
    "Ehrmann_2019" = 1,
    "Huang_2025" = 1,
    "Lan_2022" = 1,
    "Fukumoto_2023" = 1.35 * ((biological$uvp * 1.73 / biological$bsa) / 87.6)^0.67,

    # Piperacillin
    "klastrup_2020" = (2.25 + 0.119 * biological$cg_tbw),
    "Sukarnjanaset_2019" = 5.37 + (0.06 * (biological$cg_tbw - 55)), #Median Arterial Pressure disabled for the moment
    "Udy_2015" = 16.3 * (biological$cg_tbw / 100),
    # default value if no match
    1
  )

  eta_cl <- switch(model,
    # Cefepim eta CL
    "Barreto_2023" = 1, #get_sd_from_cv(0.299),
    "cacqueray_2022" = 0.39,
    "an_2023" = get_sd_from_cv(0.299),

    # Ceftazidime
    "Buning_2021" = get_sd_from_cv(0.36),
    "Launay_2024" = 0.46, #standard deviation
    "Cojutti_2024" = get_sd_from_cv(0.6792),

    # Ceftolozane
    "Chandorkar_2015" = get_sd_from_cv(0.33),
    "Zhang_2021" = get_sd_from_cv(0.429),

    # Meropenem
    "Gijsen_2021" = 1,
    "Minichmayr_2024" = 1,
    "Ehrmann_2019" = 1,
    "Huang_2025" = 1,
    "Lan_2022" = 1,
    "Fukumoto_2023" = get_sd_from_cv(0.221),

    # Piperacillin
    "klastrup_2020" = 0.533,
    "Sukarnjanaset_2019" = get_sd_from_cv(0.285),
    "Udy_2015" = get_cv_from_sd(0.56),

    # default value if no match
    1
  )

  # automatic selection of dose increment based on selected drug.
  # This dose increment is in gram
  dose_increment <- case_when(
    drug == "amoxicillin" ~ 0.500,
    drug == "cefepim" ~ 1.000,
    drug == "cefazoline" ~ 0.500,
    drug == "cefotaxim" ~ 0.500,
    drug == "ceftazidime" ~ 1.000,
    drug == "ceftaroline" ~ 1.000,
    drug == "ceftobiprol" ~ 1.000,
    drug == "ceftolozane" ~ 1.000,
    drug == "pipetazo" ~ 2.000,
    drug == "meropenem" ~ 0.500,
    TRUE ~ 0 # default value if no match
  )
  return(list(cl = cl, eta_cl = eta_cl, dose_increment = dose_increment))
}



drug_threshold <- function(drug) {
  toxicity <- switch(drug,
    "amoxicillin" = NA,
    "cefepim" = 20, # Lamoth et al. 2010
    "cefazoline" = NA,
    "cefotaxim" = NA,
    "ceftazidime" = NA,
    "ceftaroline" = NA,
    "ceftobiprol" = NA,
    "ceftolozane" = NA,
    "pipetazo" = 157,
    "meropenem" = 45, # Scharf C et al, 2020 https://pmc.ncbi.nlm.nih.gov/articles/PMC7148485/
    0
  )

  return(toxicity)
}
