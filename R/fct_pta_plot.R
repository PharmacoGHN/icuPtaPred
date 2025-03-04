#' pta_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plot.pta <- function(data, ecoff = NA) {
  pta_plot <- ggplot(data = data) + # , aes(x = .data$mic, y = .data$css_mic)) +
    geom_line(mapping = aes(x = .data$mic, y = .data$css_mic), col = "#2db391", lty = 1, lwd = 1) +
    geom_line(mapping = aes(x = .data$mic, y = .data$toxicity_threshold), col = "#960b0b") +
    # geom_hline(aes(yintercept = .data$toxicity_threshold, linetype = "Toxicity Levels"), lwd = 1, col = "#960b0b") +
    #geom_hline(aes(yintercept = 8, linetype = "Efficay Threshold"), lwd = 1, col = "red") + # to modify by EUCAST ECOFF for a given bacteria
    #geom_vline(aes(xintercept = ecoff, linetype = "ECOFF"), lwd = 1, col = "black") +
    # scale_linetype_manual(
    #   name = "Breakpoint",
    #   values = c(2, 2), # values = c(2, 2, 2),
    #   guide = guide_legend(override.aes = list(color = c("red", "black"))) # c("#960b0b", "red", "black")))
    # ) +
    labs(linetype = NULL) +
    scale_x_log10(breaks = data$mic, labels = data$mic, limits = c(max(0.01, min(data$mic)), max(data$mic))) +
    scale_y_log10(limits = c(max(0.01, min(data$css_mic_below2)), max(data$css_mic_above2))) +
    xlab("MIC (mg/L)") +
    ylab("Css/MIC") +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "inside",
      legend.justification.inside = c(0.9, 0.9),
      legend.box.background = element_rect()
    )

  if (!is.na(ecoff)) pta_plot <- pta_plot + geom_vline(aes(xintercept = ecoff, linetype = "ECOFF"), lwd = 1, col = "black")

  # Create the PTA plot with the different css/mic lines and display
  pta_multiple_doses <- pta_plot +
    geom_line(data = data, mapping = aes(x = .data$mic, y = .data$css_mic_below1), col = "#20846b", lty = 1, lwd = 1) +
    geom_line(data = data, mapping = aes(x = .data$mic, y = .data$css_mic_below2), col = "#1f8269", lty = 1, lwd = 1) +
    geom_line(data = data, mapping = aes(x = .data$mic, y = .data$css_mic_above1), col = "#32c5a0", lty = 1, lwd = 1) +
    geom_line(data = data, mapping = aes(x = .data$mic, y = .data$css_mic_above2), col = "#2fe3b6", lty = 1, lwd = 1)

  pta_ci_plot <- pta_plot +
    geom_ribbon(data = data, aes(ymin = .data$percentile_2.5, ymax = .data$percentile_97.5, x = .data$mic), fill = "#0889f1", alpha = 0.1, col = "#0889f1")

  return(list(pta_plot = pta_plot, pta_multiple_doses = pta_multiple_doses, pta_ci_plot = pta_ci_plot))
}


# add CFR plot