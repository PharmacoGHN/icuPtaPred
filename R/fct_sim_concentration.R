#' calc_css_distribution
#'
#' @description function to calculate the css distribution for a given dose and tvcl
#'
#' @noRd

calc_css_distribution <- function(
    dose,
    tvcl,
    eta_cl,
    n_sim  = 50000
  ) {
  # calculate cl and css distribution PK formula -> css = R0/CL
  set.seed(3917985)
  if (n_sim == 0) cl_distribution <- tvcl
  if (n_sim > 0) cl_distribution <- tvcl * stats::rlnorm(n_sim, meanlog = 0, sdlog = eta_cl)
  css_distribution <- (dose / 24) / cl_distribution

  return(css_distribution)
}


#' sim_concentration
#'
#' @description A function to simulate the concentration of a drug for a given dose by continuous infusion and tvcl
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

sim_concentration <- function(
  dose,
  tvcl,
  eta_cl,
  quantile = c(0.025, 0.975),
  mic = NA,
  dose_increment = 0,
  toxicity_threshold,
  n_sim = 50000
) {

  # set default mic is none is selected
  if (length(mic) == 1 && is.na(mic)) mic <- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64) # default range value

  css_distribution <- calc_css_distribution(dose, tvcl, eta_cl, n_sim)
  quant <- stats::quantile(css_distribution, probs = quantile)

  # add simulation of 2 dosing above and below if these are not 0
  dose_range <- c(-2, -1, 0, 1, 2) * dose_increment + dose
  tv_css_range <- dose_range / (tvcl * 24)

  # generate dataframe containing several dose target attainment
  css_mic_range <- data.frame(
    css_mic_below2 = tv_css_range[1] / mic,
    css_mic_below1 = tv_css_range[2] / mic,
    css_mic = tv_css_range[3] / mic,
    css_mic_above1 = tv_css_range[4] / mic,
    css_mic_above2 = tv_css_range[5] / mic
  )

  # create the output file containing css distribution summary
  quantile_df <- data.frame(
    css_mic = tv_css_range[3] / mic, # median_css / mic,
    mic = mic,
    percentile_2.5 = quant[1] / mic,
    percentile_97.5 = quant[2] / mic
  )

  # bind both data.frame
  concentration_df <- quantile_df |>
    dplyr::left_join(css_mic_range) |>
    dplyr::bind_cols(toxicity_threshold = toxicity_threshold / mic)


  return(round(concentration_df, digits = 4))
}




#' calculate_cfr
#'
#' @description A function to calculate the cfr for a given dose and mic value
#'
#' @param tvcl tvcl is the typical clearance of the drug
#' @param eta_cl eta_cl is the variability of the clearance of the drug
#' @param dose dose increment is the increment of the dose to be used in the simulation
#' @param mic_dsitrbiution mic distribution is a dataframe with mic values and their distribution derived from eucast
#' @param toxicity_threshold toxicity threshold is the toxicity threshold of the drug. Default to NULL if not known
#' @param n_sim number of simulation to be used for the calculation. Default to 50000
#'
#' @return return a dataframe containing the cfr for each dose and mic value aswell as the proportion of patients above the toxicity threshold if known
#'
#' @author Romain Garreau
#' @noRd


# # test variables
# max_dose <- 16
# dose_increment <- 2
# tvcl <- 5.5
# eta_cl <- 0.2
# mic_distribution <- data.frame(
#   mic = c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32),
#   distribution = c(10, 100, 300, 1597, 1000, 150, 3, 10, 0, 5)
# )

calculate_cfr <- function(
  tvcl,
  eta_cl,
  dose,
  mic_distribution,
  toxicity_threshold = NULL,
  n_sim = 50000
) {

  # check if mic_distribution is a dataframe
  if (!is.data.frame(mic_distribution)) {
    stop("mic_distribution must be a dataframe")
  }

  # global variables
  cfr <- 0 # Initialize cfr variable
  mic_distribution <- dplyr::mutate(mic_distribution, relative_distribution = distribution / sum(distribution))
  css_distribution <- calc_css_distribution(dose, tvcl, eta_cl)

  # calculate the cfr for a single dose and all mic values
  for (i in 1:nrow(mic_distribution)) {
    css_distribution_mic <- mean(css_distribution > mic_distribution$mic[i]) * mic_distribution$relative_distribution[i]
    cfr <- sum(cfr, css_distribution_mic)
  }

  # calculate the probabilities of being over the toxicity threshold
  if (!is.null(toxicity_threshold)) {
    toxicity_proportion <- mean(css_distribution > toxicity_threshold)
  }

  return(list(
    cfr = cfr,
    toxicity_proportion = toxicity_proportion
  ))
}


#' calculate_cfr_multiple_dose
#'
#' @description A function to calculate the cfr for a given dose and mic value
#'
#' @param tvcl tvcl is the typical clearance of the drug
#' @param eta_cl eta_cl is the variability of the clearance of the drug
#' @param dose_increment dose increment is the increment of the dose to be used in the simulation
#' @param mic_distribution mic distribution is a dataframe with mic values and their distribution derived from eucast
#' @param toxicity_threshold toxicity threshold is the toxicity threshold of the drug. Default to NULL if not known
#'
#' @return return a dataframe containing the cfr for each dose and mic value aswell as the proportion of patients above the toxicity threshold if known
#'
#' @author Romain Garreau
#' @noRd
#'
#'

calculate_cfr_mulitple_doses <- function(dose_increment, dose_max, tvcl, eta_cl, mic_distribution, toxicity_threshold = NULL, n_sim = 50000) {

  # create all dosing sequence
  dosing_sequence <- seq(0, dose_max, dose_increment)

  for (i in seq_along(dosing_sequence)) {
    # calculate the cfr for each dose
    cfr <- calculate_cfr(tvcl, eta_cl, dosing_sequence[i], mic_distribution, toxicity_threshold, n_sim)
    # create a dataframe with the cfr and the dose and append all doses in a single dataframe
    if (i == 1) {
      cfr_df <- data.frame(
        dose = dosing_sequence[i],
        cfr = cfr$cfr,
        toxicity_proportion = cfr$toxicity_proportion
      )
    } else {
      cfr_df <- rbind(cfr_df, data.frame(
        dose = dosing_sequence[i],
        cfr = cfr$cfr,
        toxicity_proportion = cfr$toxicity_proportion
      ))
    }
  }

  return(cfr_df)
}
