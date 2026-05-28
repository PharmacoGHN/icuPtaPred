box::use(
  bs4Dash[box, tabBox],
  dplyr[slice],
  plotly[ggplotly, plotlyOutput, renderPlotly],
  shiny,
  shinyvalidate[InputValidator]
)

box::use(
  app/logic/helper_model_information[model_information],
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

patient_summary_card <- function(biological, model_selected) {
  metrics <- list(
    summary_metric("CRCL (CG-TBW)", biological$cg_tbw, "mL/min", emphasis = TRUE),
    summary_metric("CRCL (CG-AJBW)", biological$cg_ajbw, "mL/min"),
    summary_metric("CKD-EPI 2021", biological$ckd_2021, "mL/min/1.73m2"),
    summary_metric("MDRD", biological$mdrd, "mL/min/1.73m2"),
    summary_metric("BMI", biological$bmi, "kg/m2"),
    summary_metric("BSA", biological$bsa, "m2", digits = 2),
    summary_metric("IBW", biological$ibw, "kg"),
    summary_metric("LBW", biological$lbw, "kg")
  )

  if (is.finite(biological$uvp) && biological$uvp > 0) {
    metrics <- append(metrics, list(summary_metric("UV/P", biological$uvp, "mL/min")))
  }

  shiny$tags$div(
    class = "icu-patient-summary",
    shiny$tags$div(
      class = "icu-patient-summary__header",
      shiny$tags$div(
        shiny$tags$span("Derived patient metrics", class = "icu-patient-summary__title"),
        shiny$tags$p(
          "Updated from the current anthropometric and renal inputs.",
          class = "icu-patient-summary__copy"
        )
      ),
      shiny$tags$span(model_selected, class = "icu-patient-summary__model")
    ),
    shiny$tags$div(class = "icu-patient-summary__grid", metrics)
  )
}

patient_summary_placeholder <- function() {
  shiny$tags$div(
    class = "icu-patient-summary icu-patient-summary--placeholder",
    shiny$tags$span("Derived patient metrics", class = "icu-patient-summary__title"),
    shiny$tags$p(
      "Compute PTA to display CRCL, body-size metrics, and the model-linked patient summary.",
      class = "icu-patient-summary__copy"
    )
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
          class = "icu-card",
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
          shiny$sliderInput(
            ns("confidence_level"),
            label = labels("conf_interval", "label", language),
            min = 0,
            max = 1,
            value = c(0.025, 0.975),
            step = 0.01
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
          tabBox(
            width = 12,
            height = "760px",
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
      ),
      shiny$column(
        width = 3,
        box(
          width = 12,
          title = shiny$tagList(shiny$icon("user-injured"), "Patient profile"),
          status = "warning",
          solidHeader = TRUE,
          class = "icu-card",
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
          shiny$numericInput(
            ns("weight"),
            label = labels("weight", "label", language),
            value = 70,
            min = 0,
            max = 500,
            step = 1
          ),
          shiny$numericInput(
            ns("creatinine"),
            label = labels("creatinine", "label", language),
            value = 60,
            min = 0,
            max = 1500,
            step = 1
          ),
          shiny$selectInput(
            ns("creatinine_unit"),
            label = "Creatinine unit",
            choices = c("mg/dL" = "mg/dL", "umol/L" = "uM/L"),
            selected = "uM/L"
          ),
          shiny$selectInput(
            ns("sex"),
            label = labels("sex", "label", language),
            choices = labels("sex", "choices", language),
            selected = "Male"
          ),
          shiny$numericInput(
            ns("urine_creatinine"),
            label = "Urinary creatinine (mmol/L)",
            value = 0,
            min = 0,
            max = 100
          ),
          shiny$numericInput(
            ns("urine_output"),
            label = "Urine output (mL / 24 h)",
            value = 0,
            min = 0,
            max = 20000,
            step = 50
          )
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
        "Weight must be in kg"
      }
    })
    validator$add_rule("weight", function(value) {
      if (value > 500) {
        "Weight must be less than 500 kg"
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
        is.null(model_information[[input$beta_lactamin]])
      ) {
        character(0)
      } else {
        names(model_information[[input$beta_lactamin]])
      }

      default_model <- pta_service$get_default_model(input$beta_lactamin)
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

    eucast <- pta_service$update_eucast()
    shiny$updateSelectInput(
      session,
      "bacteria_select",
      choices = c("Probabilistic" = "probabilist", eucast[[2]]$bacteria)
    )

    output$footer_cfr <- shiny$renderUI({
      footer_note(
        "Select a bacterium from EUCAST to compute the cumulative fraction of response."
      )
    })

    output$patient_summary <- shiny$renderUI({
      patient_summary_placeholder()
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

      distribution <- pta_service$mic_distribution(
        input$beta_lactamin,
        input$bacteria_select,
        eucast
      )

      mic_information(distribution)

      if (is.null(distribution)) {
        mic_specie(NULL)
        ecoff(NA_real_)
        ecoff_ci(NULL)
        return()
      }

      mic_specie(as.numeric(names(distribution[["mic_distribution"]])))
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

      biological <- pta_service$calc_biological(
        weight = input$weight,
        height = input$height,
        sex = input$sex,
        age = input$age,
        creatinine = input$creatinine,
        urine_creat = input$urine_creatinine,
        urine_output = input$urine_output,
        weight_unit = "kg",
        creat_unit = input$creatinine_unit
      )

      model_selected <- if (
        isTRUE(input$advanced_user_mode) &&
        !is.null(input$model_selected) &&
        nzchar(input$model_selected)
      ) {
        input$model_selected
      } else {
        pta_service$get_default_model(input$beta_lactamin)
      }

      model_param <- pta_service$get_model_parameters(
        model = model_selected,
        biological = biological,
        drug = input$beta_lactamin
      )

      toxicity_threshold <- pta_service$drug_threshold(input$beta_lactamin)
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
        quantile = input$confidence_level,
        mic = if (identical(input$bacteria_select, "probabilist")) {
          NA
        } else {
          mic_specie()
        },
        dose_increment = model_param$dose_increment * 1000,
        toxicity_threshold = toxicity_threshold,
        additional_threshold = additional_concentration
      )

      if (!identical(input$bacteria_select, "probabilist")) {
        mic_distribution_df <- data.frame(
          mic = mic_specie(),
          distribution = as.numeric(slice(mic_information()[["mic_distribution"]], 1))
        )

        cfr_df <- pta_service$calculate_cfr_mulitple_doses(
          dose_increment = model_param$dose_increment * 1000,
          dose_max = pta_service$max_dose(input$beta_lactamin) * 1000,
          tvcl = model_param$cl,
          eta_cl = model_param$eta_cl,
          mic_distribution = mic_distribution_df,
          toxicity_threshold = toxicity_threshold
        )

        cfr_plot <- pta_service$plot.cfr(cfr_df)
        output$cfr_output <- renderPlotly({
          ggplotly(cfr_plot)
        })

        output$footer_cfr <- shiny$renderUI({
          footer_note("The dashed reference lines highlight 10% and 90% CFR.")
        })
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

      pta_plot <- pta_service$plot.pta(
        concentration_df,
        ecoff = if (identical(input$bacteria_select, "probabilist")) {
          NA
        } else {
          ecoff()
        }
      )

      output$pta_output <- renderPlotly({
        ggplotly(pta_plot$pta_multiple_doses)
      })

      output$pta_output_probability <- renderPlotly({
        ggplotly(pta_plot$pta_ci_plot)
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
          return(footer_note("Confidence intervals are shown across the default MIC range."))
        }

        shiny$tags$div(
          class = "icu-footer-metrics",
          shiny$tags$span(shiny$tags$b("ECOFF:"), ecoff(), " mg/L"),
          shiny$tags$span(shiny$tags$b("Confidence interval:"), ecoff_ci())
        )
      })

      output$patient_summary <- shiny$renderUI({
        patient_summary_card(biological, model_selected)
      })
    })
  })
}