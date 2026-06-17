box::use(
  dplyr[mutate],
  stats[quantile]
)

DEFAULT_MIC_PLOT_GRID <- c(
  0.001, 0.002, 0.004, 0.008, 0.016, 0.03, 0.06,
  0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64,
  128, 256, 512, 1024
)


# ==================================================== #
#                       Helper functions
# ==================================================== #

calc_css_distribution <- function(
  dose,
  tvcl,
  eta_cl,
  n_sim  = 3000,
  concentration_multiplier = 1
) {
  if (!is.finite(concentration_multiplier) || concentration_multiplier <= 0) {
    stop("concentration_multiplier must be a positive numeric value")
  }

  # calculate cl and css distribution PK formula -> css = R0/CL
  set.seed(3917985)
  if (n_sim == 0) cl_distribution <- tvcl
  if (n_sim > 0) cl_distribution <- tvcl * stats::rlnorm(n_sim, meanlog = 0, sdlog = eta_cl)

  if (length(dose) == 1) {
    css_distribution <- ((dose / 24) / cl_distribution) * concentration_multiplier
  }

  if (length(dose) > 1) {
    css_distribution <- matrix(NA, nrow = n_sim, ncol = length(dose))
    for (i in seq_along(dose)) {
      css_distribution[, i] <- ((dose[i] / 24) / cl_distribution) * concentration_multiplier
    }
  }

  return(css_distribution)
}

# Convert an absolute concentration threshold into a threshold-to-MIC curve.
threshold_curve <- function(threshold, mic) {
  if (length(threshold) == 1 && (is.na(threshold) || threshold <= 0)) {
    return(rep(NA_real_, length(mic)))
  }

  threshold / mic
}

toxicity_threshold_marker <- function(threshold) {
  if (length(threshold) == 0 || is.na(threshold) || threshold <= 0) {
    return(NA_real_)
  }

  threshold
}

# Remove values that cannot be displayed on the log-scaled PTA plots.
sanitize_log_series <- function(values) {
  values[!is.finite(values) | values <= 0] <- NA_real_
  values
}


# ==================================================== #
#                       Core functions
# ==================================================== #

#' Expand sparse MIC observations into a continuous plotting grid.
#'
#' @param mic Observed MIC values.
#' @param dilution_series Canonical MIC dilution values to use for the plot grid.
#'
#' @return A sorted numeric vector spanning the observed MIC range.
#' @export
build_plot_mic_grid <- function(mic, dilution_series = DEFAULT_MIC_PLOT_GRID) {
  positive_mic <- sort(unique(mic[is.finite(mic) & !is.na(mic) & mic > 0]))

  if (!length(positive_mic)) {
    return(c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64))
  }

  dilution_grid <- dilution_series[
    dilution_series >= min(positive_mic) &
      dilution_series <= max(positive_mic)
  ]

  sort(unique(c(positive_mic, dilution_grid)))
}

#' Simulate steady-state concentration-to-MIC ratios for a dose regimen.
#'
#' @param dose Daily dose in milligrams.
#' @param tvcl Typical clearance in liters per hour.
#' @param eta_cl Inter-individual variability for clearance on the log scale.
#' @param quantile Probability interval to report for the simulated distribution.
#' @param css_quantile Percentile used for the default PTA curve at each dose.
#' @param mic MIC values to evaluate. When omitted, a default doubling dilution grid is used.
#' @param dose_increment Daily dose increment in milligrams used for neighboring regimen curves.
#' @param toxicity_threshold Optional concentration threshold in mg/L.
#' @param additional_threshold Optional secondary concentration threshold in mg/L.
#' @param n_sim Number of virtual patients to simulate.
#'
#' @return A data frame with the default percentile-based PTA curve, neighboring
#'   dose curves, probability interval bounds, and threshold curves.
#' @export
sim_concentration <- function(
  dose,
  tvcl,
  eta_cl,
  quantile = c(0.025, 0.975),
  css_quantile = 0.95,
  mic = NA,
  dose_increment = 0,
  toxicity_threshold,
  additional_threshold = NA_real_,
  n_sim = 50000,
  concentration_multiplier = 1
) {
  if (length(mic) == 1 && is.na(mic)) {
    mic <- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64)
  }

  css_distribution <- calc_css_distribution(dose, tvcl, eta_cl, n_sim, concentration_multiplier)
  quant <- quantile(css_distribution, probs = quantile)
  dose_range <- c(-2, -1, 0, 1, 2) * dose_increment + dose

  # Use the same percentile summary for the selected dose and its neighbors so
  # the default PTA curve reflects the requested concentration distribution.
  all_dose_css_distribution <- calc_css_distribution(dose_range, tvcl, eta_cl, n_sim, concentration_multiplier)
  quant_all_dose <- apply(all_dose_css_distribution, 2, function(x) quantile(x, probs = css_quantile))

  concentration_df <- data.frame(
    css_mic = sanitize_log_series(quant_all_dose[3] / mic),
    mic = mic,
    percentile_2.5 = sanitize_log_series(quant[1] / mic),
    percentile_97.5 = sanitize_log_series(quant[2] / mic),
    css_mic_below2 = sanitize_log_series(quant_all_dose[1] / mic),
    css_mic_below1 = sanitize_log_series(quant_all_dose[2] / mic),
    css_mic_above1 = sanitize_log_series(quant_all_dose[4] / mic),
    css_mic_above2 = sanitize_log_series(quant_all_dose[5] / mic),
    toxicity_threshold = rep(
      toxicity_threshold_marker(toxicity_threshold * concentration_multiplier),
      length(mic)
    ),
    additional_threshold = sanitize_log_series(threshold_curve(additional_threshold * concentration_multiplier, mic))
  )

  round(concentration_df, digits = 4)
}

calculate_cfr <- function(
  tvcl,
  eta_cl,
  dose,
  mic_distribution,
  toxicity_threshold = NULL,
  n_sim = 50000,
  concentration_multiplier = 1
) {
  if (!is.data.frame(mic_distribution)) {
    stop("mic_distribution must be a dataframe")
  }

  cfr <- 0
  mic_distribution <- mutate(
    mic_distribution,
    relative_distribution = distribution / sum(distribution)
  )
  css_distribution <- calc_css_distribution(
    dose,
    tvcl,
    eta_cl,
    n_sim = n_sim,
    concentration_multiplier = concentration_multiplier
  )

  for (index in seq_len(nrow(mic_distribution))) {
    css_distribution_mic <- mean(css_distribution > mic_distribution$mic[index])
    css_distribution_mic <- css_distribution_mic * mic_distribution$relative_distribution[index]
    cfr <- sum(cfr, css_distribution_mic)
  }

  toxicity_proportion <- NULL
  if (!is.null(toxicity_threshold) && is.finite(toxicity_threshold) && toxicity_threshold > 0) {
    toxicity_proportion <- mean(css_distribution > (toxicity_threshold * concentration_multiplier))
  }

  list(cfr = cfr, toxicity_proportion = toxicity_proportion)
}

#' Compute CFR across a dose range for a given MIC distribution.
#'
#' @param dose_increment Dose step in milligrams.
#' @param dose_max Maximum daily dose in milligrams.
#' @param tvcl Typical clearance in liters per hour.
#' @param eta_cl Inter-individual variability for clearance on the log scale.
#' @param mic_distribution A data frame with `mic` and `distribution` columns.
#' @param toxicity_threshold Optional concentration threshold in mg/L.
#' @param n_sim Number of virtual patients to simulate.
#'
#' @return A data frame with one row per tested dose and the corresponding CFR and toxicity proportion.
#' @export
calculate_cfr_mulitple_doses <- function(
  dose_increment,
  dose_max,
  tvcl,
  eta_cl,
  mic_distribution,
  toxicity_threshold = NULL,
  n_sim = 50000,
  concentration_multiplier = 1
) {
  dosing_sequence <- seq(0, dose_max, dose_increment)

  for (index in seq_along(dosing_sequence)) {
    cfr <- calculate_cfr(
      tvcl,
      eta_cl,
      dosing_sequence[index],
      mic_distribution,
      toxicity_threshold,
      n_sim,
      concentration_multiplier
    )

    current_row <- data.frame(
      dose = dosing_sequence[index],
      cfr = cfr$cfr,
      toxicity_proportion = cfr$toxicity_proportion
    )

    if (index == 1) {
      cfr_df <- current_row
    } else {
      cfr_df <- rbind(cfr_df, current_row)
    }
  }

  cfr_df
}