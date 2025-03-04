#' sim_concentration
#'
#' @description A fct function
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
  toxicity_threshold
) {

  # set default mic is none is selected
  if (length(mic) == 1 && is.na(mic)) mic <- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64) # default range value

  # typical css over 24 hours infusion rate
  tv_css <- dose / (tvcl * 24)
  css_mic <- tv_css / mic

  # calculate cl and css distribution PK formula -> css = R0/CL
  set.seed(16897)
  cl_distribution <- stats::rnorm(100000, mean = tvcl, sd = eta_cl) # TODO CL distrubution is logNormal To update
  css_distribution <- dose / (cl_distribution * 24)
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
    css_mic = css_mic,
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


# TODO add CFR calculation