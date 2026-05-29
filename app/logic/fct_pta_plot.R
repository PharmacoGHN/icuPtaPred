box::use(
  dplyr,
  ggplot2,
  scales
)

positive_finite_values <- function(...) {
  values <- unlist(list(...), use.names = FALSE)
  values[is.finite(values) & !is.na(values) & values > 0]
}

# Format numeric values for hover labels while keeping missing values explicit.
format_plot_value <- function(value, digits = 6) {
  if (length(value) == 0) {
    return(character())
  }

  formatted_value <- rep("NA", length(value))
  valid_values <- is.finite(value) & !is.na(value)

  formatted_value[valid_values] <- format(signif(value[valid_values], digits), scientific = FALSE, trim = TRUE)

  formatted_value
}

# Build a readable label for the selected regimen and neighboring dose steps.
dose_curve_label <- function(step, selected_dose = NA_real_, dose_increment = NA_real_) {
  if (is.finite(selected_dose) && is.finite(dose_increment) && dose_increment > 0) {
    dose_value <- selected_dose + step * dose_increment

    if (step == 0) {
      return(paste0("Selected regimen (", format_plot_value(dose_value), " g/day)"))
    }

    direction <- if (step < 0) "below" else "above"
    step_count <- abs(step)

    return(
      paste0(
        step_count,
        " dose step",
        if (step_count > 1) "s" else "",
        " ", direction,
        " selected regimen (",
        format_plot_value(dose_value),
        " g/day)"
      )
    )
  }

  if (step == 0) {
    return("Selected regimen")
  }

  paste0(abs(step), " dose step", if (abs(step) > 1) "s" else "", if (step < 0) " below" else " above", " selected regimen")
}

pta_hover_text <- function(mic, ratio, label) {
  paste0(
    "MIC: ", format_plot_value(mic), " mg/L",
    "<br>", label,
    "<br> Css/MIC ratio: ", format_plot_value(ratio)
  )
}

pta_interval_hover_text <- function(mic, lower, upper, label) {
  paste0(
    "MIC: ", format_plot_value(mic), " mg/L",
    "<br>", label,
    "<br>95% interval: ", format_plot_value(lower), " to ", format_plot_value(upper), " Css/MIC"
  )
}

threshold_hover_text <- function(mic, threshold, label) {
  paste0(
    "MIC: ", format_plot_value(mic), " mg/L",
    "<br>", label,
    "<br>Threshold concentration: ", format_plot_value(threshold), " mg/L"
  )
}

safe_log_limits <- function(..., lower_floor = 0.01, fallback_upper = 1) {
  values <- positive_finite_values(...)

  if (!length(values)) {
    return(c(lower_floor, fallback_upper))
  }

  lower <- max(lower_floor, min(values))
  upper <- max(values)

  if (!is.finite(upper) || upper <= lower) {
    upper <- max(lower * 2, fallback_upper)
  }

  c(lower, upper)
}

#' Build PTA plots for the selected regimen, neighboring doses, and uncertainty band.
#'
#' @param data A data frame produced by `sim_concentration()` with the MIC axis,
#'   steady-state concentration ratios, and threshold columns.
#' @param ecoff Optional ECOFF value to show as a vertical reference line.
#' @param selected_dose The currently selected daily dose in grams.
#' @param dose_increment The daily dose increment in grams used for neighboring curves.
#'
#' @return A named list with `pta_plot`, `pta_multiple_doses`, and `pta_ci_plot`
#'   ggplot objects.
#' @export
plot.pta <- function(data, ecoff = NA, selected_dose = NA_real_, dose_increment = NA_real_) {
  data <- dplyr$mutate(
    data,
    selected_hover = mapply(pta_hover_text, .data$mic, .data$css_mic, MoreArgs = list(label = dose_curve_label(0, selected_dose, dose_increment))),
    below1_hover = mapply(pta_hover_text, .data$mic, .data$css_mic_below1, MoreArgs = list(label = dose_curve_label(-1, selected_dose, dose_increment))),
    below2_hover = mapply(pta_hover_text, .data$mic, .data$css_mic_below2, MoreArgs = list(label = dose_curve_label(-2, selected_dose, dose_increment))),
    above1_hover = mapply(pta_hover_text, .data$mic, .data$css_mic_above1, MoreArgs = list(label = dose_curve_label(1, selected_dose, dose_increment))),
    above2_hover = mapply(pta_hover_text, .data$mic, .data$css_mic_above2, MoreArgs = list(label = dose_curve_label(2, selected_dose, dose_increment))),
    probability_hover = mapply(
      pta_interval_hover_text,
      .data$mic,
      .data$percentile_2.5,
      .data$percentile_97.5,
      MoreArgs = list(label = paste0(dose_curve_label(0, selected_dose, dose_increment), " probability interval"))
    ),
    toxicity_hover = mapply(
      threshold_hover_text,
      .data$mic,
      .data$toxicity_threshold,
      MoreArgs = list(label = "Toxicity threshold")
    ),
    additional_hover = mapply(
      threshold_hover_text,
      .data$mic,
      .data$additional_threshold,
      MoreArgs = list(label = "Additional concentration threshold")
    )
  )

  x_limits <- safe_log_limits(data$mic)
  y_limits <- safe_log_limits(
    data$css_mic_below2,
    data$css_mic_below1,
    data$css_mic,
    data$css_mic_above1,
    data$css_mic_above2,
    data$percentile_2.5,
    data$percentile_97.5,
    data$toxicity_threshold,
    data$additional_threshold
  )

  pta_plot <- ggplot2$ggplot(data = data) +
    ggplot2$geom_hline(mapping = ggplot2$aes(yintercept = 1), col = "#2b94ab", lty = 2, lwd = 0.5) +
    ggplot2$geom_hline(mapping = ggplot2$aes(yintercept = 4), col = "#0e877b", lty = 2, lwd = 0.5) +
    ggplot2$geom_line(mapping = ggplot2$aes(x = .data$mic, y = .data$css_mic, text = .data$selected_hover, group = 1), col = "#2db391", lty = 1, lwd = 1, na.rm = TRUE) +
    ggplot2$labs(linetype = NULL) +
    ggplot2$scale_x_continuous(
      trans = "log2",
      breaks = data$mic,
      labels = data$mic,
      limits = x_limits
    ) +
    ggplot2$scale_y_continuous(
      trans = "log2",
      n.breaks = 10,
      limits = y_limits
    ) +
    ggplot2$xlab("Minimum inhibitory concentration (MIC, mg/L)") +
    ggplot2$ylab("Steady-state concentration to MIC ratio") +
    ggplot2$theme_bw(base_size = 14) +
    ggplot2$theme(
      legend.position = "inside",
      legend.justification.inside = c(0.9, 0.9),
      legend.box.background = ggplot2$element_rect()
    )

  if (any(is.finite(data$toxicity_threshold) & !is.na(data$toxicity_threshold))) {
    pta_plot <- pta_plot + ggplot2$geom_line(
      mapping = ggplot2$aes(x = .data$mic, y = .data$toxicity_threshold, text = .data$toxicity_hover, group = 1),
      col = "#960b0b",
      lty = 1,
      lwd = 0.9,
      na.rm = TRUE
    )
  }

  if (any(is.finite(data$additional_threshold) & !is.na(data$additional_threshold))) {
    pta_plot <- pta_plot + ggplot2$geom_line(
      mapping = ggplot2$aes(x = .data$mic, y = .data$additional_threshold, text = .data$additional_hover, group = 1),
      col = "#f2a65a",
      lty = 3,
      lwd = 0.9,
      na.rm = TRUE
    )
  }

  if (!is.na(ecoff)) {
    pta_plot <- pta_plot + ggplot2$geom_vline(
      ggplot2$aes(xintercept = ecoff, linetype = "ECOFF"),
      lwd = 1,
      col = "black"
    )
  }

  pta_multiple_doses <- pta_plot +
    ggplot2$geom_line(
      data = data,
      mapping = ggplot2$aes(x = .data$mic, y = .data$css_mic_below1, text = .data$below1_hover, group = 1),
      col = "#20846b",
      lty = 1,
      lwd = 1,
      na.rm = TRUE
    ) +
    ggplot2$geom_line(
      data = data,
      mapping = ggplot2$aes(x = .data$mic, y = .data$css_mic_below2, text = .data$below2_hover, group = 1),
      col = "#1f8269",
      lty = 1,
      lwd = 1,
      na.rm = TRUE
    ) +
    ggplot2$geom_line(
      data = data,
      mapping = ggplot2$aes(x = .data$mic, y = .data$css_mic_above1, text = .data$above1_hover, group = 1),
      col = "#32c5a0",
      lty = 1,
      lwd = 1,
      na.rm = TRUE
    ) +
    ggplot2$geom_line(
      data = data,
      mapping = ggplot2$aes(x = .data$mic, y = .data$css_mic_above2, text = .data$above2_hover, group = 1),
      col = "#2fe3b6",
      lty = 1,
      lwd = 1,
      na.rm = TRUE
    )

  pta_ci_plot <- pta_plot +
    ggplot2$geom_ribbon(
      data = data,
      ggplot2$aes(
        ymin = .data$percentile_2.5,
        ymax = .data$percentile_97.5,
        x = .data$mic,
        text = .data$probability_hover,
        group = 1
      ),
      fill = "#0889f1",
      alpha = 0.1,
      col = "#0889f1",
      na.rm = TRUE
    )

  list(
    pta_plot = pta_plot,
    pta_multiple_doses = pta_multiple_doses,
    pta_ci_plot = pta_ci_plot
  )
}

#' Build the CFR plot for a sequence of daily doses.
#'
#' @param data A data frame with `dose`, `cfr`, and `toxicity_proportion` columns.
#' @param dose_increment Reserved for compatibility with older call sites.
#'
#' @return A ggplot object showing CFR and, when available, toxicity risk.
#' @export
plot.cfr <- function(data, dose_increment = 0) {
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }

  required_columns <- c("dose", "cfr", "toxicity_proportion")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "The data frame is missing the following columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  data <- dplyr$mutate(
    data,
    cfr_hover = paste0(
      "Daily dose: ", format_plot_value(.data$dose / 1000), " g/day",
      "<br>Cumulative fraction of response: ", format_plot_value(.data$cfr * 100), "%"
    ),
    toxicity_hover = paste0(
      "Daily dose: ", format_plot_value(.data$dose / 1000), " g/day",
      "<br>Toxicity risk: ", format_plot_value(.data$toxicity_proportion * 100), "%"
    )
  )

  cfr_plot <- ggplot2$ggplot(data, ggplot2$aes(x = .data$dose / 1000)) +
    ggplot2$geom_line(ggplot2$aes(y = .data$cfr, text = .data$cfr_hover, group = 1), col = "#2db391", lty = 1, lwd = 1)

  if (any(is.finite(data$toxicity_proportion) & !is.na(data$toxicity_proportion))) {
    cfr_plot <- cfr_plot + ggplot2$geom_line(
      ggplot2$aes(y = .data$toxicity_proportion, text = .data$toxicity_hover, group = 1),
      col = "#960b0b"
    )
  }

  cfr_plot +
    ggplot2$scale_x_continuous(trans = scales$pseudo_log_trans()) +
    ggplot2$xlab("Daily dose (g/day)") +
    ggplot2$ylab("Cumulative fraction of response") +
    ggplot2$geom_hline(yintercept = 0.1, col = "#2b94ab", lty = 2, lwd = 0.5) +
    ggplot2$geom_hline(yintercept = 0.9, col = "#0e877b", lty = 2, lwd = 0.5) +
    ggplot2$theme_bw(base_size = 14) +
    ggplot2$theme(
      legend.position = "inside",
      legend.justification.inside = c(0.9, 0.9),
      legend.box.background = ggplot2$element_rect()
    )
}
