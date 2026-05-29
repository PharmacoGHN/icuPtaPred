box::use(
  bs4Dash[box, tabBox],
  plotly[config, ggplotly, layout, plotlyOutput, renderPlotly],
  shiny,
  shinyvalidate[InputValidator]
)

box::use(
  app/logic/fct_calc_biological[calc_biological],
  app/logic/fct_extract_eucast[mic_distribution, read_eucast_mic, update_eucast],
  app/logic/fct_pta_plot,
  app/logic/model_registry[get_default_model, get_model_definition, get_model_parameters, list_models_for_drug],
  app/logic/pta_service,
  app/logic/utils[labels]
)

dose_badges <- function(all_dose) {
  dose_colors <- c("#1f8269", "#20846b", "#2db391", "#32c5a0", "#2fe3b6")

  shiny$tags$div(
    class = "icu-dose-badges",
    lapply(seq_along(all_dose), function(index) {
      if (all_dose[index] <= 0) {
        return(NULL)
      }

      shiny$tags$span(
        class = "icu-dose-badge",
        style = paste0("background-color:", dose_colors[index], ";"),
        paste0(all_dose[index], " g")
      )
    })
  )
}

footer_note <- function(text) {
  shiny$tags$div(class = "icu-footer-note", text)
}

concentration_badge <- function(value) {
  if (!is.finite(value) || value <= 0) {
    return(NULL)
  }

  shiny$tags$div(
    class = "icu-target-badges",
    shiny$tags$span(
      class = "icu-target-badge",
      paste0("Plotted concentration: ", round(value, 1), " mg/L")
    )
  )
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

  exponents <- seq(floor(log2(min(positive_values))), ceiling(log2(max(positive_values))))
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

pta_plotly <- function(plot, data) {
  x_tick_values <- sort(unique(data$mic[is.finite(data$mic) & !is.na(data$mic) & data$mic > 0]))
  x_tick_positions <- log2(x_tick_values)
  x_range <- log2_axis_range(data$mic)
  y_values <- unlist(
    data[c(
      "css_mic_below2",
      "css_mic_below1",
      "css_mic",
      "css_mic_above1",
      "css_mic_above2",
      "percentile_2.5",
      "percentile_97.5",
      "toxicity_threshold",
      "additional_threshold"
    )],
    use.names = FALSE
  )
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

summary_metric <- function(label, value, unit = NULL, digits = 1, emphasis = FALSE) {
  metric_value <- if (is.null(value) || !is.finite(value)) {
    "NA"
  } else {
    format(round(value, digits), trim = TRUE, scientific = FALSE)
  }

  if (!is.null(unit) && metric_value != "NA") {
    metric_value <- paste(metric_value, unit)
  }

  shiny$tags$div(
    class = paste(
      c("icu-summary-metric", if (emphasis) "icu-summary-metric--emphasis" else NULL),
      collapse = " "
    ),
    shiny$tags$span(label, class = "icu-summary-metric__label"),
    shiny$tags$span(metric_value, class = "icu-summary-metric__value")
  )
}

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

  shiny$tags$div(
    class = "icu-patient-summary",
    shiny$tags$span("Patient information", class = "icu-patient-summary__title"),
    shiny$tags$p(
      paste0("Model renal formula: ", model_param$renal_formula, "."),
      class = "icu-patient-summary__copy"
    ),
    if (isTRUE(model_param$used_manual_renal) && is.finite(model_param$renal_value)) {
      shiny$tags$p(
        "Manual override is active for the selected model.",
        class = "icu-patient-summary__copy"
      )
    },
    shiny$tags$div(class = "icu-patient-summary__grid", metrics)
  )
}

patient_summary_placeholder <- function() {
  shiny$tags$div(
    class = "icu-patient-summary icu-patient-summary--placeholder",
    shiny$tags$span("Patient information", class = "icu-patient-summary__title"),
    shiny$tags$p(
      "Compute PTA to display the model renal formula, the renal value used, and the derived weight values.",
      class = "icu-patient-summary__copy"
    )
  )
}

renal_formula_note <- function(drug, model, manual_renal_function = NA_real_) {
  if (is.null(drug) || !nzchar(drug)) {
    return(
      shiny$tags$div(
        class = "icu-renal-note icu-renal-note--placeholder",
        shiny$tags$span("Renal function source", class = "icu-renal-note__title"),
        shiny$tags$p(
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

  shiny$tags$div(
    class = "icu-renal-note",
    shiny$tags$span("Renal function source", class = "icu-renal-note__title"),
    shiny$tags$p(copy, class = "icu-renal-note__copy")
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  language <- "fr"

  shiny$tagList(
    shiny$fluidRow(
      shiny$column(
        width = 3,
        box(
          width = 12,
          title = shiny$tagList(shiny$icon("vial"), "Pathogen and regimen"),
          status = "primary",
          solidHeader = TRUE,
          class = "icu-card icu-card--controls",
          shiny$selectInput(
            ns("bacteria_select"),
            "Bacterium",
            choices = c("Probabilistic" = "probabilist")
          ),
          shiny$selectInput(
            ns("beta_lactamin"),
            label = labels("drug", "label", language),
            choices = labels("drug", "choices", language),
            selected = character(0)
          ),
          shiny$numericInput(
            ns("drug_dose"),
            label = labels("dose_input", "label", language),
            value = 0,
            step = 0.125,
            min = 0,
            max = 32
          ),
          shiny$numericInput(
            ns("additional_concentration"),
            label = "Additional concentration to plot (mg/L)",
            value = 0,
            min = 0,
            step = 0.5
          ),
          shiny$checkboxInput(
            ns("advanced_user_mode"),
            "Advanced user mode",
            value = FALSE
          ),
          shiny$conditionalPanel(
            condition = sprintf("input['%s']", ns("advanced_user_mode")),
            shiny$selectInput(
              ns("model_selected"),
              "Population PK model",
              choices = character(0)
            )
          ),
          shiny$tags$p(
            "The probability interval is fixed at 95%.",
            class = "icu-inline-note"
          ),
          shiny$actionButton(
            ns("compute_pta"),
            "Compute PTA",
            class = "icu-primary-button"
          ),
          shiny$uiOutput(ns("patient_summary"))
        )
      ),
      shiny$column(
        width = 6,
        box(
          width = 12,
          title = shiny$tagList(shiny$icon("chart-area"), "Simulation outputs"),
          status = "success",
          solidHeader = TRUE,
          class = "icu-card",
          shiny$div(
            class = "icu-output-tabs",
            tabBox(
              width = 12,
              height = "760px",
              type = "tabs",
              background = "white",
              solidHeader = FALSE,
              collapsible = FALSE,
              selected = "Dose-response",
              shiny$tabPanel(
                title = "Dose-response",
                plotlyOutput(ns("pta_output"), height = "620px"),
                shiny$uiOutput(ns("footer_pta"))
              ),
              shiny$tabPanel(
                title = "Probability interval",
                plotlyOutput(ns("pta_output_probability"), height = "620px"),
                shiny$uiOutput(ns("footer_pta_probability"))
              ),
              shiny$tabPanel(
                title = "CFR",
                plotlyOutput(ns("cfr_output"), height = "620px"),
                shiny$uiOutput(ns("footer_cfr"))
              )
            )
          )
        )
      ),
      shiny$column(
        width = 3,
        box(
          width = 12,
          title = shiny$tagList(shiny$icon("user-injured"), "Patient profile"),
          status = "warning",
          solidHeader = TRUE,
          class = "icu-card icu-card--controls",
          shiny$numericInput(
            ns("age"),
            label = labels("age", "label", language),
            value = 18,
            min = 0,
            max = 120,
            step = 1
          ),
          shiny$numericInput(
            ns("height"),
            label = labels("height", "label", language),
            value = 180,
            min = 0,
            max = 250,
            step = 1
          ),
          shiny$fluidRow(
            shiny$column(
              width = 8,
              shiny$numericInput(
                ns("weight"),
                label = labels("weight", "label", language),
                value = 70,
                min = 0,
                max = 1100,
                step = 1
              )
            ),
            shiny$column(
              width = 4,
              shiny$selectInput(
                ns("weight_unit"),
                label = "Unit",
                choices = c("kg" = "kg", "lbs" = "lbs"),
                selected = "kg"
              )
            )
          ),
          shiny$fluidRow(
            shiny$column(
              width = 8,
              shiny$numericInput(
                ns("creatinine"),
                label = labels("creatinine", "label", language),
                value = 60,
                min = 0,
                max = 1500,
                step = 1
              )
            ),
            shiny$column(
              width = 4,
              shiny$selectInput(
                ns("creatinine_unit"),
                label = "Unit",
                choices = c("mg/dL" = "mg/dL", "umol/L" = "uM/L"),
                selected = "uM/L"
              )
            )
          ),
          shiny$selectInput(
            ns("sex"),
            label = labels("sex", "label", language),
            choices = labels("sex", "choices", language),
            selected = "Male"
          ),
          shiny$fluidRow(
            shiny$column(
              width = 6,
              shiny$numericInput(
                ns("urine_creatinine"),
                label = "Urinary creatinine (mmol/L)",
                value = 0,
                min = 0,
                max = 100
              )
            ),
            shiny$column(
              width = 6,
              shiny$numericInput(
                ns("urine_output"),
                label = "Urine output (mL / 24 h)",
                value = 0,
                min = 0,
                max = 20000,
                step = 50
              )
            )
          ),
          shiny$numericInput(
            ns("manual_renal_function"),
            label = "Manual renal function override (mL/min)",
            value = NA_real_,
            min = 0,
            step = 1
          )
          ,
          shiny$uiOutput(ns("renal_function_method"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    mic_information <- shiny$reactiveVal(NULL)
    mic_specie <- shiny$reactiveVal(NULL)
    ecoff <- shiny$reactiveVal(NA_real_)
    ecoff_ci <- shiny$reactiveVal(NULL)

    validator <- InputValidator$new()
    validator$add_rule("drug_dose", function(value) {
      if (value == 0) {
        "Dose must be greater than 0"
      }
    })
    validator$add_rule("height", function(value) {
      if (value < 10) {
        "Height must be in cm"
      }
    })
    validator$add_rule("height", function(value) {
      if (value > 250) {
        "Height must be less than 250 cm"
      }
    })
    validator$add_rule("weight", function(value) {
      if (value < 1) {
        "Weight must be greater than 0"
      }
    })
    validator$add_rule("weight", function(value) {
      max_weight <- if (identical(input$weight_unit, "lbs")) 1100 else 500

      if (value > max_weight) {
        paste0("Weight must be less than ", max_weight, if (identical(input$weight_unit, "lbs")) " lbs" else " kg")
      }
    })
    validator$add_rule("age", function(value) {
      if (value <= 0) {
        "Age must be greater than 0"
      }
    })
    validator$add_rule("age", function(value) {
      if (value > 120) {
        "Age must be less than 120"
      }
    })
    validator$add_rule("manual_renal_function", function(value) {
      if (!is.na(value) && value < 0) {
        "Manual renal function must be greater than or equal to 0"
      }
    })
    validator$add_rule("beta_lactamin", function(value) {
      if (is.null(value) || !nzchar(value)) {
        "Choose a drug before computing PTA"
      }
    })
    validator$add_rule("model_selected", function(value) {
      if (isTRUE(input$advanced_user_mode) && (is.null(value) || !nzchar(value))) {
        "Choose a model when advanced user mode is enabled"
      }
    })
    validator$enable()

    shiny$observeEvent(input$beta_lactamin, {
      model_choices <- if (
        is.null(input$beta_lactamin) ||
        !nzchar(input$beta_lactamin) ||
        !length(list_models_for_drug(input$beta_lactamin))
      ) {
        character(0)
      } else {
        list_models_for_drug(input$beta_lactamin)
      }

      default_model <- get_default_model(input$beta_lactamin)
      selected_model <- if (length(model_choices) && default_model %in% model_choices) {
        default_model
      } else if (length(model_choices)) {
        model_choices[[1]]
      } else {
        character(0)
      }

      shiny$updateSelectInput(
        session,
        "model_selected",
        choices = model_choices,
        selected = selected_model
      )
    }, ignoreInit = FALSE)

    eucast <- update_eucast()
    eucast_mic <- read_eucast_mic()
    shiny$updateSelectInput(
      session,
      "bacteria_select",
      choices = c("Probabilistic" = "probabilist", eucast[[2]]$bacteria)
    )

    output$footer_cfr <- shiny$renderUI({
      footer_note(
        if (length(eucast_mic)) {
          "Select a bacterium from EUCAST to compute the cumulative fraction of response."
        } else {
          "EUCAST MIC data are read from app/static/eucast.json and app/static/eucast_mic.json. Populate the MIC cache manually to enable CFR."
        }
      )
    })

    output$patient_summary <- shiny$renderUI({
      patient_summary_placeholder()
    })

    selected_model <- shiny$reactive({
      if (isTRUE(input$advanced_user_mode) && !is.null(input$model_selected) && nzchar(input$model_selected)) {
        return(input$model_selected)
      }

      if (is.null(input$beta_lactamin) || !nzchar(input$beta_lactamin)) {
        return(character(0))
      }

      get_default_model(input$beta_lactamin)
    })

    output$renal_function_method <- shiny$renderUI({
      renal_formula_note(
        drug = input$beta_lactamin,
        model = selected_model(),
        manual_renal_function = input$manual_renal_function
      )
    })

    shiny$observeEvent(list(input$bacteria_select, input$beta_lactamin), {
      if (
        identical(input$bacteria_select, "probabilist") ||
        is.null(input$beta_lactamin) ||
        !nzchar(input$beta_lactamin)
      ) {
        mic_information(NULL)
        mic_specie(NULL)
        ecoff(NA_real_)
        ecoff_ci(NULL)
        return()
      }

      distribution <- mic_distribution(
        input$beta_lactamin,
        input$bacteria_select,
        eucast_mic
      )

      mic_information(distribution)

      if (is.null(distribution)) {
        mic_specie(NULL)
        ecoff(NA_real_)
        ecoff_ci(NULL)
        return()
      }

      mic_specie(distribution[["mic_distribution"]]$mic)
      ecoff(as.numeric(distribution$ecoff))
      ecoff_ci(distribution$ecoff_ci)
    }, ignoreInit = FALSE)

    shiny$observeEvent(input$compute_pta, {
      if (!validator$is_valid()) {
        shiny$showNotification(
          "Please fix the highlighted inputs before continuing.",
          duration = 8,
          type = "error",
          closeButton = TRUE
        )
        return()
      }

      if (
        !identical(input$bacteria_select, "probabilist") &&
        is.null(mic_information())
      ) {
        shiny$showNotification(
          "No MIC distribution is available for the selected bacterium.",
          duration = 8,
          type = "error",
          closeButton = TRUE
        )
        return()
      }

      biological <- calc_biological(
        weight = input$weight,
        height = input$height,
        sex = input$sex,
        age = input$age,
        creatinine = input$creatinine,
        urine_creat = input$urine_creatinine,
        urine_output = input$urine_output,
        weight_unit = input$weight_unit,
        creat_unit = input$creatinine_unit
      )

      model_selected <- selected_model()

      model_param <- get_model_parameters(
        model = model_selected,
        biological = biological,
        drug = input$beta_lactamin,
        manual_renal_function = input$manual_renal_function
      )

      toxicity_threshold <- model_param$toxicity_threshold
      additional_concentration <- if (
        is.finite(input$additional_concentration) && input$additional_concentration > 0
      ) {
        input$additional_concentration
      } else {
        NA_real_
      }

      concentration_df <- pta_service$sim_concentration(
        dose = input$drug_dose * 1000,
        tvcl = model_param$cl,
        eta_cl = model_param$eta_cl,
        quantile = c(0.025, 0.975),
        mic = if (identical(input$bacteria_select, "probabilist")) {
          NA
        } else {
          pta_service$build_plot_mic_grid(mic_specie())
        },
        dose_increment = model_param$dose_increment * 1000,
        toxicity_threshold = toxicity_threshold,
        additional_threshold = additional_concentration
      )

      if (!identical(input$bacteria_select, "probabilist")) {
        mic_distribution_df <- mic_information()[["mic_distribution"]]

        if (!is.null(mic_distribution_df) && nrow(mic_distribution_df) > 0) {
          mic_distribution_df <- mic_distribution_df[
            is.finite(mic_distribution_df$mic) &
              is.finite(mic_distribution_df$distribution) &
              mic_distribution_df$distribution > 0,
            ,
            drop = FALSE
          ]
        } else {
          mic_distribution_df <- data.frame(mic = numeric(0), distribution = numeric(0))
        }

        if (nrow(mic_distribution_df) > 0) {
          cfr_df <- pta_service$calculate_cfr_mulitple_doses(
            dose_increment = model_param$dose_increment * 1000,
            dose_max = model_param$max_dose * 1000,
            tvcl = model_param$cl,
            eta_cl = model_param$eta_cl,
            mic_distribution = mic_distribution_df,
            toxicity_threshold = toxicity_threshold
          )

          cfr_plot <- fct_pta_plot$plot.cfr(cfr_df)
          output$cfr_output <- renderPlotly({
            cfr_plotly(cfr_plot)
          })

          output$footer_cfr <- shiny$renderUI({
            footer_note("The dashed reference lines highlight 10% and 90% CFR.")
          })
        } else {
          output$cfr_output <- renderPlotly({
            NULL
          })

          output$footer_cfr <- shiny$renderUI({
            footer_note("The selected bacterium does not expose a usable MIC distribution for CFR computation.")
          })
        }
      } else {
        output$cfr_output <- renderPlotly({
          NULL
        })

        output$footer_cfr <- shiny$renderUI({
          footer_note(
            "Select a bacterium from EUCAST to compute the cumulative fraction of response."
          )
        })
      }

      pta_plot <- fct_pta_plot$plot.pta(
        concentration_df,
        ecoff = if (identical(input$bacteria_select, "probabilist")) {
          NA
        } else {
          ecoff()
        },
        selected_dose = input$drug_dose,
        dose_increment = model_param$dose_increment
      )

      output$pta_output <- renderPlotly({
        pta_plotly(pta_plot$pta_multiple_doses, concentration_df)
      })

      output$pta_output_probability <- renderPlotly({
        pta_plotly(pta_plot$pta_ci_plot, concentration_df)
      })

      output$footer_pta <- shiny$renderUI({
        all_dose <- c(-2, -1, 0, 1, 2) * model_param$dose_increment + input$drug_dose
        shiny$tagList(
          dose_badges(all_dose),
          concentration_badge(additional_concentration)
        )
      })

      output$footer_pta_probability <- shiny$renderUI({
        if (identical(input$bacteria_select, "probabilist")) {
          return(footer_note("Fixed 95% confidence intervals are shown across the default MIC range."))
        }

        shiny$tags$div(
          class = "icu-footer-metrics",
          shiny$tags$span(shiny$tags$b("Probability interval:"), "95%"),
          shiny$tags$span(shiny$tags$b("ECOFF:"), ecoff(), " mg/L"),
          shiny$tags$span(shiny$tags$b("Confidence interval:"), ecoff_ci())
        )
      })

      output$patient_summary <- shiny$renderUI({
        patient_summary_card(biological, model_param)
      })
    })
  })
}