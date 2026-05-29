box::use(
  dplyr,
  stats
)

calc_css_distribution <- function(dose, tvcl, eta_cl, n_sim = 50000) {
  set.seed(3917985)

  if (n_sim == 0) {
    cl_distribution <- tvcl
  }

  if (n_sim > 0) {
    cl_distribution <- tvcl * stats$rlnorm(n_sim, meanlog = 0, sdlog = eta_cl)
  }

  (dose / 24) / cl_distribution
}

threshold_curve <- function(threshold, mic) {
  if (length(threshold) == 1 && (is.na(threshold) || threshold <= 0)) {
    return(rep(NA_real_, length(mic)))
  }

  threshold / mic
}

sanitize_log_series <- function(values) {
  values[!is.finite(values) | values <= 0] <- NA_real_
  values
}

#' Simulate steady-state concentration-to-MIC ratios for a dose regimen.
#'
#' @param dose Daily dose in milligrams.
#' @param tvcl Typical clearance in liters per hour.
#' @param eta_cl Inter-individual variability for clearance on the log scale.
#' @param quantile Probability interval to report for the simulated distribution.
#' @param mic MIC values to evaluate. When omitted, a default doubling dilution grid is used.
#' @param dose_increment Daily dose increment in milligrams used for neighboring regimen curves.
#' @param toxicity_threshold Optional concentration threshold in mg/L.
#' @param additional_threshold Optional secondary concentration threshold in mg/L.
#' @param n_sim Number of virtual patients to simulate.
#'
#' @return A data frame with concentration-to-MIC ratios, uncertainty bounds, and threshold curves.
#' @export
sim_concentration <- function(
  dose,
  tvcl,
  eta_cl,
  quantile = c(0.025, 0.975),
  mic = NA,
  dose_increment = 0,
  toxicity_threshold,
  additional_threshold = NA_real_,
  n_sim = 50000
) {
  if (length(mic) == 1 && is.na(mic)) {
    mic <- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64)
  }

  css_distribution <- calc_css_distribution(dose, tvcl, eta_cl, n_sim)
  quant <- stats$quantile(css_distribution, probs = quantile)
  dose_range <- c(-2, -1, 0, 1, 2) * dose_increment + dose
  tv_css_range <- dose_range / (tvcl * 24)

  css_mic_range <- data.frame(
    css_mic_below2 = sanitize_log_series(tv_css_range[1] / mic),
    css_mic_below1 = sanitize_log_series(tv_css_range[2] / mic),
    css_mic_above1 = sanitize_log_series(tv_css_range[4] / mic),
    css_mic_above2 = sanitize_log_series(tv_css_range[5] / mic)
  )

  quantile_df <- data.frame(
    css_mic = sanitize_log_series(tv_css_range[3] / mic),
    mic = mic,
    percentile_2.5 = sanitize_log_series(quant[1] / mic),
    percentile_97.5 = sanitize_log_series(quant[2] / mic)
  )

  concentration_df <- dplyr$bind_cols(
    quantile_df,
    css_mic_range,
    toxicity_threshold = sanitize_log_series(threshold_curve(toxicity_threshold, mic)),
    additional_threshold = sanitize_log_series(threshold_curve(additional_threshold, mic))
  )

  round(concentration_df, digits = 4)
}

calculate_cfr <- function(
  tvcl,
  eta_cl,
  dose,
  mic_distribution,
  toxicity_threshold = NULL,
  n_sim = 50000
) {
  if (!is.data.frame(mic_distribution)) {
    stop("mic_distribution must be a dataframe")
  }

  cfr <- 0
  mic_distribution <- dplyr$mutate(
    mic_distribution,
    relative_distribution = distribution / sum(distribution)
  )
  css_distribution <- calc_css_distribution(dose, tvcl, eta_cl, n_sim = n_sim)

  for (index in seq_len(nrow(mic_distribution))) {
    css_distribution_mic <- mean(css_distribution > mic_distribution$mic[index])
    css_distribution_mic <- css_distribution_mic * mic_distribution$relative_distribution[index]
    cfr <- sum(cfr, css_distribution_mic)
  }

  toxicity_proportion <- NULL
  if (!is.null(toxicity_threshold) && is.finite(toxicity_threshold) && toxicity_threshold > 0) {
    toxicity_proportion <- mean(css_distribution > toxicity_threshold)
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
  n_sim = 50000
) {
  dosing_sequence <- seq(0, dose_max, dose_increment)

  for (index in seq_along(dosing_sequence)) {
    cfr <- calculate_cfr(
      tvcl,
      eta_cl,
      dosing_sequence[index],
      mic_distribution,
      toxicity_threshold,
      n_sim
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