#' pta_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @import ggplot2
#' @noRd

plot.pta <- function(data, ecoff = NA) {
  pta_plot <- ggplot(data = data) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), col = "#2b94ab", lty = 2, lwd = 0.5) + # Css>MIC
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 4), col = "#0e877b", lty = 2, lwd = 0.5) + # Css > 4 x MIC
    ggplot2::geom_line(mapping = ggplot2::aes(x = .data$mic, y = .data$css_mic), col = "#2db391", lty = 1, lwd = 1) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = .data$mic, y = .data$toxicity_threshold), col = "#960b0b") +
    ggplot2::labs(linetype = NULL) +
    ggplot2::scale_x_continuous(trans = "log2", breaks = data$mic, labels = data$mic, limits = c(max(0.01, min(data$mic)), max(data$mic))) +
    ggplot2::scale_y_continuous(trans = "log2", n.breaks = 10, limits = c(max(0.01, min(data$css_mic_below2)), max(data$css_mic_above2))) +
    ggplot2::xlab("MIC (mg/L)") +
    ggplot2::ylab("Css/MIC") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      legend.position = "inside",
      legend.justification.inside = c(0.9, 0.9),
      legend.box.background = ggplot2::element_rect()
    )

  if (!is.na(ecoff)) pta_plot <- pta_plot + ggplot2::geom_vline(ggplot2::aes(xintercept = ecoff, linetype = "ECOFF"), lwd = 1, col = "black")

  # Create the PTA plot with the different css/mic lines and display
  pta_multiple_doses <- pta_plot +
    ggplot2::geom_line(data = data, mapping = ggplot2::aes(x = .data$mic, y = .data$css_mic_below1), col = "#20846b", lty = 1, lwd = 1) +
    ggplot2::geom_line(data = data, mapping = ggplot2::aes(x = .data$mic, y = .data$css_mic_below2), col = "#1f8269", lty = 1, lwd = 1) +
    ggplot2::geom_line(data = data, mapping = ggplot2::aes(x = .data$mic, y = .data$css_mic_above1), col = "#32c5a0", lty = 1, lwd = 1) +
    ggplot2::geom_line(data = data, mapping = ggplot2::aes(x = .data$mic, y = .data$css_mic_above2), col = "#2fe3b6", lty = 1, lwd = 1)

  pta_ci_plot <- pta_plot +
    ggplot2::geom_ribbon(data = data, ggplot2::aes(ymin = .data$percentile_2.5, ymax = .data$percentile_97.5, x = .data$mic), fill = "#0889f1", alpha = 0.1, col = "#0889f1")

  return(list(pta_plot = pta_plot, pta_multiple_doses = pta_multiple_doses, pta_ci_plot = pta_ci_plot))
}


# add CFR plot
#' plot.cfr
#'
#' @description
#' Plot the cumulative fraction rate based on eucast value if available
#'
#' @param data take a data.frame with the following columns: dose, cfr, toxicity_proportion
#' 
#' @import ggplot2
#' @export
#' @author Romain Garreau
#'
#'


plot.cfr <- function(data) {

  # Check if the data is a data frame
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }

  # check if the data has the required columns
  required_columns <- c("dose", "cfr", "toxicity_proportion")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The data frame is missing the following columns:", paste(missing_columns, collapse = ", ")))
  }

  #TODO add cfr Css = 4xMIC
  # create cfr plot
  cfr_plot <- ggplot(data, aes(x = .data$dose)) +
    geom_line(aes(y = .data$cfr), col = "#2db391", lty = 1, lwd = 1) +
    geom_line(aes(y = .data$toxicity_proportion), col = "#960b0b") +
    xlab("Dose (g)") +
    ylab("CFR (%)") +
    geom_hline(yintercept = 0.1, col = "#2b94ab", lty = 2, lwd = 0.5) +
    geom_hline(yintercept = 0.9, col = "#0e877b", lty = 2, lwd = 0.5) +
    theme_bw(base_size = 14) +
    ggplot2::theme(
      legend.position = "inside",
      legend.justification.inside = c(0.9, 0.9),
      legend.box.background = ggplot2::element_rect()
    )

  return(cfr_plot)
}
