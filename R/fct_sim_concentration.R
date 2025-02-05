#' sim_concentration
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

sim_concentration <- function(dose, tvcl, eta_cl, quantile = c(0.025, 0.975), mic = NA) {

  # set default mic is none is selected
  if (length(mic) == 1 && is.na(mic)) mic <- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64) # default range value

  # typical css over 24 hours infusion rate
  tv_css <- dose / (tvcl * 24)
  css_mic <- tv_css / mic

  # calculate cl and css distribution PK formula -> css = R0/CL
  set.seed(16897)
  cl_distribution <- stats::rnorm(100000, mean = tvcl, sd = eta_cl)
  css_distribution <- dose / (cl_distribution * 24)
  quant <- stats::quantile(css_distribution, probs = quantile)

  # create the output file containing css distribution summary
  concentration_df <- data.frame(
    css_mic = css_mic,
    mic = mic,
    percentile_2.5 = quant[1] / mic,
    percentile_97.5 = quant[2] / mic#,
    # css_mic_above2 <- css_mic_values[1],
    # css_mic_above1 <- css_mic_values[2],
    # css_mic_below1 <- css_mic_values[3],
    # css_mic_below2 <- css_mic_values[4]
  )

  return(round(concentration_df, digits = 1))
}
