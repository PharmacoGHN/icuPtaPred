box::use(
  dplyr[mutate],
  ggplot2[aes, ggplot, geom_line, geom_hline, geom_ribbon, labs, scale_x_continuous, scale_y_continuous, xlab, ylab, theme_bw, theme, element_rect],
  plotly[config, ggplotly, layout],
  scales
)

# ==================================================== #
# Global constants for PTA plot data frame column names
# ==================================================== #
PTA_RATIO_COLUMNS <- c(below2 = "css_mic_below2", below1 = "css_mic_below1", selected = "css_mic", above1 = "css_mic_above1", above2 = "css_mic_above2")
PTA_INTERVAL_COLUMNS <- c(lower = "percentile_2.5", upper = "percentile_97.5")
PTA_THRESHOLD_COLUMNS <- c(toxicity_threshold = "Toxicity threshold", additional_threshold = "Additional concentration threshold")
PTA_PLOT_VALUE_COLUMNS <- c(  unname(PTA_RATIO_COLUMNS),  unname(PTA_INTERVAL_COLUMNS),  names(PTA_THRESHOLD_COLUMNS))



# ==================================================== #
# Helper functions for PTA plot construction
# ==================================================== #

positive_finite_values <- function(...) {
  values <- unlist(list(...), use.names = FALSE)
  values[is.finite(values) & !is.na(values) & values > 0]
}



# Format numeric hover values consistently while keeping missing values explicit.
format_plot_value <- function(value, digits = 6) {
  if (length(value) == 0) {
    return(character())
  }

  formatted_value <- rep("NA", length(value))
  valid_values <- is.finite(value) & !is.na(value)

  formatted_value[valid_values] <- format(
    signif(value[valid_values], digits),
    scientific = FALSE,
    trim = TRUE
  )

  formatted_value
}



dose_curve_label <- function(
  step,
  selected_dose = NA_real_,
  dose_increment = NA_real_
) {
  if (
    is.finite(selected_dose) && is.finite(dose_increment) && dose_increment > 0
  ) {
    dose_value <- selected_dose + step * dose_increment

    if (step == 0) {
      return(paste0("Selected regimen (", format_plot_value(dose_value), " g/day)"))
    }

    direction <- if (step < 0) "below" else "above"
    step_count <- abs(step)

    return(
      paste0(step_count, " dose step", if (step_count > 1) "s" else "", " ", direction, " selected regimen (", format_plot_value(dose_value), " g/day)" )
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
  paste0("MIC: ", format_plot_value(mic), " mg/L",
    "<br>", label,
    "<br>Threshold concentration: ", format_plot_value(threshold), " mg/L"
  )
}

# Check whether a curve has at least one value that can be shown on the plot.
has_plot_values <- function(values) {
  any(is.finite(values) & !is.na(values))
}

# Collect the numeric series that should contribute to log-scale axis limits.
extract_plot_values <- function(data, columns) {
  available_columns <- intersect(columns, colnames(data))

  if (!length(available_columns)) {
    return(numeric(0))
  }

  unlist(data[available_columns], use.names = FALSE)
}



validate_pta_plot_data <- function(data) {
  required_columns <- c("mic", unname(PTA_RATIO_COLUMNS), unname(PTA_INTERVAL_COLUMNS), names(PTA_THRESHOLD_COLUMNS))
  missing_columns <- setdiff(required_columns, colnames(data))

  if (length(missing_columns) > 0) {
    stop(paste("The PTA plot data is missing the following columns:", paste(missing_columns, collapse = ", ")))
  }

  invisible(data)
}



build_ratio_hover <- function(mic, values, label) {
  mapply(pta_hover_text, mic, values, MoreArgs = list(label = label), USE.NAMES = FALSE)
}



build_interval_hover <- function(mic, lower, upper, label) {
  mapply(pta_interval_hover_text, mic, lower, upper, MoreArgs = list(label = label), USE.NAMES = FALSE )
}



build_threshold_hover <- function(mic, values, label) {
  mapply(threshold_hover_text, mic, values, MoreArgs = list(label = label), USE.NAMES = FALSE)
}

# Attach all hover labels up front so the plot construction code only handles geoms.
build_pta_plot_data <- function(data, selected_dose, dose_increment) {
  validate_pta_plot_data(data)

  data$selected_hover <- build_ratio_hover(data$mic, data[[PTA_RATIO_COLUMNS[["selected"]]]], dose_curve_label(0, selected_dose, dose_increment))
  data$below1_hover <- build_ratio_hover(data$mic, data[[PTA_RATIO_COLUMNS[["below1"]]]], dose_curve_label(-1, selected_dose, dose_increment))
  data$below2_hover <- build_ratio_hover(data$mic, data[[PTA_RATIO_COLUMNS[["below2"]]]], dose_curve_label(-2, selected_dose, dose_increment))
  data$above1_hover <- build_ratio_hover(data$mic, data[[PTA_RATIO_COLUMNS[["above1"]]]], dose_curve_label(1, selected_dose, dose_increment))
  data$above2_hover <- build_ratio_hover(data$mic, data[[PTA_RATIO_COLUMNS[["above2"]]]], dose_curve_label(2, selected_dose, dose_increment))
  data$probability_hover <- build_interval_hover(
    data$mic, data[[PTA_INTERVAL_COLUMNS[["lower"]]]], data[[PTA_INTERVAL_COLUMNS[["upper"]]]],
    paste0(dose_curve_label(0, selected_dose, dose_increment), " probability interval")
  )

  data$toxicity_hover <- build_threshold_hover(data$mic, data$toxicity_threshold, PTA_THRESHOLD_COLUMNS[["toxicity_threshold"]])
  data$additional_hover <- build_threshold_hover(data$mic, data$additional_threshold, PTA_THRESHOLD_COLUMNS[["additional_threshold"]])

  #return the data frame with all hover labels attached for use in plotting functions
  data
}



safe_log_limits <- function(..., fallback_upper = 1) {
  values <- positive_finite_values(...)

  if (!length(values)) {
    return(c(0.01, fallback_upper))
  }

  lower <- min(values)
  upper <- max(values)

  if (!is.finite(upper) || upper <= lower) {
    upper <- max(lower * 2, fallback_upper)
  }

  c(lower, upper)
}



format_plot_number <- function(value, digits = 6) {
  if (!is.finite(value) || is.na(value)) {
    return("NA")
  }

  format(signif(value, digits), scientific = FALSE, trim = TRUE)
}



log2_tick_values <- function(values) {
  positive_values <- values[is.finite(values) & !is.na(values) & values > 0]

  if (!length(positive_values)) {
    return(numeric(0))
  }

  exponents <- seq(
    floor(log2(min(positive_values))),
    ceiling(log2(max(positive_values)))
  )
  2^exponents
}



log2_tick_positions <- function(values) {
  tick_values <- log2_tick_values(values)

  if (!length(tick_values)) {
    return(numeric(0))
  }

  log2(tick_values)
}



log2_axis_range <- function(values) {
  positive_values <- values[is.finite(values) & !is.na(values) & values > 0]

  if (!length(positive_values)) {
    return(c(0, 1))
  }

  lower <- log2(min(positive_values))
  upper <- log2(max(positive_values))

  if (!is.finite(upper) || upper <= lower) {
    upper <- lower + 1
  }

  c(lower, upper)
}

# Add optional threshold curves only when the series contains plottable values.
add_optional_pta_line <- function(
  plot,
  data,
  value_column,
  hover_column,
  colour,
  line_type = 1,
  line_width = 0.9
) {
  if (!has_plot_values(data[[value_column]])) {
    return(plot)
  }

  plot +
    geom_line(
      data = data,
      mapping = aes(
        x = .data$mic,
        y = .data[[value_column]],
        text = .data[[hover_column]],
        group = 1
      ),
      col = colour,
      lty = line_type,
      lwd = line_width,
      na.rm = TRUE
    )
}


# =================================================== #
# Functions for building the Shiny dashboard UI
# =================================================== #

#' Build PTA plots for the selected regimen, neighboring doses, and uncertainty band.
#'
#' @param data A data frame produced by `sim_concentration()` with the MIC axis,
#'   percentile-based steady-state concentration ratios, and threshold columns.
#' @param ecoff Optional ECOFF value to show as a vertical reference line.
#' @param selected_dose The currently selected daily dose in grams.
#' @param dose_increment The daily dose increment in grams used for neighboring curves.
#'
#' @return A named list with `pta_plot`, `pta_multiple_doses`, and `pta_ci_plot`
#'   ggplot objects.
#' @export
plot.pta <- function(
  data,
  ecoff = NA,
  selected_dose = NA_real_,
  dose_increment = NA_real_
) {
  data <- build_pta_plot_data(data, selected_dose, dose_increment)

  x_limits <- safe_log_limits(data$mic)
  y_limits <- safe_log_limits(extract_plot_values(data, PTA_PLOT_VALUE_COLUMNS))

  pta_plot <- ggplot(data = data) +
    geom_hline(mapping = aes(yintercept = 1), col = "#2b94ab", lty = 2, lwd = 0.5) +
    geom_hline(mapping = aes(yintercept = 4), col = "#0e877b", lty = 2, lwd = 0.5) +
    geom_line(
      mapping = aes(x = .data$mic, y = .data$css_mic, text = .data$selected_hover, group = 1),
      col = "#2db391", lty = 1, lwd = 1, na.rm = TRUE
    ) +
    labs(linetype = NULL) +
    scale_x_continuous(trans = "log2", breaks = data$mic, labels = data$mic, limits = x_limits) +
    scale_y_continuous(trans = "log2", n.breaks = 10, limits = y_limits) +
    xlab("Minimum inhibitory concentration (MIC, mg/L)") +
    ylab("Steady-state concentration to MIC ratio") +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "inside",
      legend.justification.inside = c(0.9, 0.9),
      legend.box.background = element_rect()
    )

  pta_plot <- add_optional_pta_line(
    pta_plot,
    data,
    "toxicity_threshold",
    "toxicity_hover",
    "#960b0b"
  )

  pta_plot <- add_optional_pta_line(
    pta_plot,
    data,
    "additional_threshold",
    "additional_hover",
    "#f2a65a",
    line_type = 3
  )

  if (!is.na(ecoff)) {
    pta_plot <- pta_plot +
      geom_vline(aes(xintercept = ecoff, linetype = "ECOFF"), lwd = 1, col = "black")
  }

  pta_multiple_doses <- pta_plot +
    geom_line(
      data = data, mapping = aes(x = .data$mic, y = .data$css_mic_below1, text = .data$below1_hover, group = 1),
      col = "#20846b", lty = 1, lwd = 1, na.rm = TRUE
    ) +
    geom_line(
      data = data, mapping = aes(x = .data$mic, y = .data$css_mic_below2, text = .data$below2_hover, group = 1),
      col = "#1f8269", lty = 1, lwd = 1, na.rm = TRUE
    ) +
    geom_line(
      data = data, mapping = aes(x = .data$mic, y = .data$css_mic_above1, text = .data$above1_hover, group = 1),
      col = "#32c5a0", lty = 1, lwd = 1, na.rm = TRUE
    ) +
    geom_line(
      data = data, mapping = aes(x = .data$mic, y = .data$css_mic_above2, text = .data$above2_hover, group = 1),
      col = "#2fe3b6", lty = 1, lwd = 1, na.rm = TRUE
    )

  pta_ci_plot <- pta_plot +
    geom_ribbon(
      data = data, aes(ymin = .data$percentile_2.5, ymax = .data$percentile_97.5, x = .data$mic, text = .data$probability_hover, group = 1),
      fill = "#0889f1", alpha = 0.1, col = NA, na.rm = TRUE
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
  if (!is.data.frame(data)) stop("The input data must be a data frame.")

  required_columns <- c("dose", "cfr", "toxicity_proportion")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The data frame is missing the following columns:", paste(missing_columns, collapse = ", ")))
  }

  data <- mutate(
    data,
    cfr_hover = paste0("Daily dose: ", format_plot_value(.data$dose / 1000), " g/day", "<br>Cumulative fraction of response: ", format_plot_value(.data$cfr * 100), "%"),
    toxicity_hover = paste0("Daily dose: ", format_plot_value(.data$dose / 1000), " g/day", "<br>Toxicity risk: ", format_plot_value(.data$toxicity_proportion * 100), "%")
  )

  cfr_plot <- ggplot(data, aes(x = .data$dose / 1000)) +
    geom_line(aes(y = .data$cfr, text = .data$cfr_hover, group = 1), col = "#2db391", lty = 1, lwd = 1)

  if (any(is.finite(data$toxicity_proportion) & !is.na(data$toxicity_proportion))) {
    cfr_plot <- cfr_plot +
      geom_line(aes(y = .data$toxicity_proportion, text = .data$toxicity_hover, group = 1 ), col = "#960b0b")
  }

  cfr_plot +
    scale_x_continuous(trans = scales$pseudo_log_trans()) +
    xlab("Daily dose (g/day)") +
    ylab("Cumulative fraction of response") +
    geom_hline(yintercept = 0.1, col = "#2b94ab", lty = 2, lwd = 0.5) +
    geom_hline(yintercept = 0.9, col = "#0e877b", lty = 2, lwd = 0.5) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "inside",
      legend.justification.inside = c(0.9, 0.9),
      legend.box.background = element_rect()
    )
}

#' Convert a PTA ggplot into a log-scaled plotly widget with custom ticks.
#'
#' @param plot A ggplot produced by `plot.pta()`.
#' @param data The PTA data frame used to build the plot.
#'
#' @return A plotly object with log2 MIC and Css/MIC axes.
#' @export
pta_plotly <- function(plot, data) {
  x_tick_values <- sort(unique(data$mic[is.finite(data$mic) & !is.na(data$mic) & data$mic > 0]))
  x_tick_positions <- log2(x_tick_values)
  x_range <- log2_axis_range(data$mic)
  y_values <- extract_plot_values(data, PTA_PLOT_VALUE_COLUMNS)
  y_tick_values <- log2_tick_values(y_values)
  y_tick_positions <- log2_tick_positions(y_values)
  y_range <- log2_axis_range(y_values)

  plotly_object <- suppressWarnings(ggplotly(plot, tooltip = "text"))
  
  plotly_object <- layout(
    plotly_object,
    hovermode = "closest",
    xaxis = list(
      title = list(text = "Minimum inhibitory concentration (MIC, mg/L)"),
      autorange = FALSE,
      range = x_range,
      tickmode = "array",
      tickvals = x_tick_positions,
      ticktext = vapply(x_tick_values, format_plot_number, character(1)),
      exponentformat = "none",
      showexponent = "none"
    ),
    yaxis = list(
      title = list(text = "Steady-state concentration to MIC ratio"),
      autorange = FALSE,
      range = y_range,
      tickmode = "array",
      tickvals = y_tick_positions,
      ticktext = vapply(y_tick_values, format_plot_number, character(1)),
      exponentformat = "none",
      showexponent = "none"
    )
  )

  config(
    plotly_object,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "lasso2d",
      "select2d",
      "zoomIn2d",
      "zoomOut2d",
      "autoScale2d",
      "toggleSpikelines"
    )
  )
}

#' Convert a CFR ggplot into a plotly widget.
#'
#' @param plot A ggplot produced by `plot.cfr()`.
#'
#' @return A plotly object for the CFR panel.
#' @export
cfr_plotly <- function(plot) {
  plotly_object <- suppressWarnings(ggplotly(plot, tooltip = "text"))
  plotly_object <- layout(
    plotly_object,
    hovermode = "closest",
    xaxis = list(title = list(text = "Daily dose (g/day)")),
    yaxis = list(
      title = list(text = "Cumulative fraction of response"),
      tickformat = ".0%"
    )
  )

  config(
    plotly_object,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "lasso2d",
      "select2d",
      "zoomIn2d",
      "zoomOut2d",
      "autoScale2d",
      "toggleSpikelines"
    )
  )
}
