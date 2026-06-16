box::use(
  shiny[icon, tags]
)

box::use(
  app/logic/model_registry[get_model_definition]
)

# ==================================================== #
# UI Helper functions for PTA tab
# ==================================================== #

#' @export
dose_badges <- function(all_dose) {
  dose_colors <- c("#1f8269", "#20846b", "#2db391", "#32c5a0", "#2fe3b6")

  tags$div(
    class = "icu-dose-badges",
    lapply(seq_along(all_dose), function(index) {
      if (all_dose[index] <= 0) {
        return(NULL)
      }

      tags$span(
        class = "icu-dose-badge",
        style = paste0("background-color:", dose_colors[index], ";"),
        paste0(all_dose[index], " g")
      )
    })
  )
}

#' @export
footer_note <- function(text) {
  tags$div(class = "icu-footer-note", text)
}

#' @export
concentration_badge <- function(value) {
  if (!is.finite(value) || value <= 0) {
    return(NULL)
  }

  tags$div(
    class = "icu-target-badges",
    tags$span(
      class = "icu-target-badge",
      paste0("Plotted concentration: ", round(value, 1), " mg/L")
    )
  )
}

summary_metric <- function(label, value, unit = NULL, digits = 1, emphasis = FALSE) {
  metric_value <- if (is.null(value) || !is.finite(value)) {
    "NA"
  } else {
    format(round(value, digits), trim = TRUE, scientific = FALSE)
  }

  if (!is.null(unit) && metric_value != "NA") {
    metric_value <- paste(metric_value, unit)
  }

  tags$div(
    class = paste(
      c("icu-summary-metric", if (emphasis) "icu-summary-metric--emphasis" else NULL),
      collapse = " "
    ),
    tags$span(label, class = "icu-summary-metric__label"),
    tags$span(metric_value, class = "icu-summary-metric__value")
  )
}

#' @export
patient_summary_card <- function(biological, model_param) {
  calculated_renal_value <- if (
    !is.null(model_param$renal_metric) &&
    !identical(model_param$renal_metric, "none")
  ) {
    biological[[model_param$renal_metric]]
  } else {
    NA_real_
  }

  metrics <- list(
    summary_metric("TBW", biological$tbw, "kg", emphasis = TRUE),
    summary_metric("IBW", biological$ibw, "kg"),
    summary_metric("AJBW", biological$ajbw, "kg"),
    summary_metric("LBW", biological$lbw, "kg")
  )

  if (is.finite(calculated_renal_value)) {
    metrics <- append(
      metrics,
      list(summary_metric(paste0(model_param$renal_formula, " value"), calculated_renal_value, "mL/min", emphasis = TRUE))
    )
  }

  if (isTRUE(model_param$used_manual_renal) && is.finite(model_param$renal_value)) {
    metrics <- append(metrics, list(summary_metric("Manual override", model_param$renal_value, "mL/min")))
  }

  tags$div(
    class = "icu-patient-summary",
    tags$span("Patient information", class = "icu-patient-summary__title"),
    tags$p(
      paste0("Model renal formula: ", model_param$renal_formula, "."),
      class = "icu-patient-summary__copy"
    ),
    if (isTRUE(model_param$used_manual_renal) && is.finite(model_param$renal_value)) {
      tags$p(
        "Manual override is active for the selected model.",
        class = "icu-patient-summary__copy"
      )
    },
    tags$div(class = "icu-patient-summary__grid", metrics)
  )
}

#' @export
patient_summary_placeholder <- function() {
  tags$div(
    class = "icu-patient-summary icu-patient-summary--placeholder",
    tags$span("Patient information", class = "icu-patient-summary__title"),
    tags$p(
      "Compute PTA to display the model renal formula, the renal value used, and the derived weight values.",
      class = "icu-patient-summary__copy"
    )
  )
}

#' @export
renal_formula_note <- function(drug, model, manual_renal_function = NA_real_) {
  if (is.null(drug) || !nzchar(drug)) {
    return(
      tags$div(
        class = "icu-renal-note icu-renal-note--placeholder",
        tags$span("Renal function source", class = "icu-renal-note__title"),
        tags$p(
          "Select a drug to display the renal function method used by the current model.",
          class = "icu-renal-note__copy"
        )
      )
    )
  }

  model_definition <- get_model_definition(drug = drug, model = model)

  copy <- if (model_definition$renal_metric[[1]] == "none") {
    "This model does not use a renal function formula."
  } else if (is.finite(manual_renal_function) && manual_renal_function > 0) {
    paste0("Manual override will replace ", model_definition$renal_formula[[1]], ".")
  } else {
    paste0("This model uses ", model_definition$renal_formula[[1]], ".")
  }

  tags$div(
    class = "icu-renal-note",
    tags$span("Renal function source", class = "icu-renal-note__title"),
    tags$p(copy, class = "icu-renal-note__copy")
  )
}

#' @export
advanced_model_label <- function(model_definition = NULL) {
  warning_icon <- if (
    !is.null(model_definition) &&
    nrow(model_definition) &&
    isTRUE(model_definition$is_not_available[[1]])
  ) {
    tags$span(
      class = "icu-select-label__warning",
      icon("triangle-exclamation")
    )
  } else {
    NULL
  }

  tags$label(
    class = "control-label icu-select-label",
    tags$span("Population PK model"),
    warning_icon
  )
}

#' @export
advanced_model_warning_note <- function(model_definition = NULL) {
  if (
    is.null(model_definition) ||
    !nrow(model_definition) ||
    !isTRUE(model_definition$is_not_available[[1]])
  ) {
    return(NULL)
  }

  tags$div(
    class = "icu-select-warning",
    tags$span(
      class = "icu-select-warning__icon",
      icon("triangle-exclamation")
    ),
    tags$p(
      paste0(
        "This selected model requires caution. See the model documentation for ",
        model_definition$model[[1]],
        " in the Model Library."
      ),
      class = "icu-select-warning__copy"
    )
  )
}