box::use(
  bs4Dash[box, tabBox],
  plotly[plotlyOutput, renderPlotly],
  shiny[
    actionButton, column, checkboxInput, conditionalPanel, div, fluidRow, icon,
    moduleServer, NS, numericInput, observeEvent, reactive, reactiveVal, renderUI,
    selectInput, tabPanel, tags, tagList, uiOutput, updateSelectInput, updateNumericInput, showNotification
  ],
  shinyvalidate[InputValidator]
)

box::use(
  app/logic/fct_calc_biological[calc_biological],
  app/logic/fct_extract_eucast[mic_distribution, read_eucast_mic, update_eucast],
  app/logic/model_registry[get_default_model, get_model_definition, get_model_parameters, list_models_for_drug],
  app/logic/pta_simulation[build_plot_mic_grid, calculate_cfr_mulitple_doses, sim_concentration],
  app/logic/utils[labels],
  app/logic/pta_helper[
    advanced_model_label, advanced_model_warning_note, concentration_badge, dose_badges, footer_note,
    patient_summary_card, patient_summary_placeholder, renal_formula_note, toxicity_badge
  ],
  app/logic/pta_plot[cfr_plotly, plot.cfr, plot.pta, pta_plotly]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  language <- "fr"

  tagList(
    fluidRow(
      column(
        width = 3,
        box(
          width = 12,
          title = tagList(icon("vial"), "Pathogen and regimen"),
          status = "primary",
          solidHeader = TRUE,
          class = "icu-card icu-card--controls",
          selectInput(ns("bacteria_select"), "Bacterium", choices = c("Probabilistic" = "probabilist")),
          selectInput(ns("beta_lactamin"), label = labels("drug", "label", language), choices = labels("drug", "choices", language), selected = character(0)),
          numericInput(ns("drug_dose"), label = labels("dose_input", "label", language), value = 0, step = 0.125, min = 0, max = 32),
          numericInput(ns("additional_concentration"), label = "Additional concentration to plot (mg/L)", value = 0, min = 0, step = 0.5),
          fluidRow(
            column(width = 6, checkboxInput(ns("advanced_user_mode"), "Advanced user mode", value = FALSE)),
            column(width = 5, offset = 1,
              conditionalPanel(
                condition = sprintf("input['%s']", ns("advanced_user_mode")),
                tagList(
                  checkboxInput(ns("use_free_fraction"), "Use free fraction", value = TRUE),
                  numericInput(ns("concentration_percentile"), "Concentration percentile", value = 0.95, min = 0, max = 1, step = 0.01)
                )
              )
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("advanced_user_mode")),
            uiOutput(ns("model_selected_label")),
            selectInput(ns("model_selected"), NULL, choices = character(0)),
            uiOutput(ns("model_selected_warning"))
          ),
          tags$p(
            "The probability interval is fixed at 95%.",
            class = "icu-inline-note"
          ),
          actionButton(ns("compute_pta"), "Compute PTA", class = "icu-primary-button"),
          uiOutput(ns("patient_summary"))
        )
      ),
      column(
        width = 6,
        box(
          width = 12,
          title = tagList(icon("chart-area"), "Simulation outputs"),
          status = "success",
          solidHeader = TRUE,
          class = "icu-card",
          div(
            class = "icu-output-tabs",
            tabBox(
              width = 12,
              height = "760px",
              type = "tabs",
              background = "white",
              solidHeader = FALSE,
              collapsible = FALSE,
              selected = "Dose-response",
              tabPanel(
                title = "Dose-response",
                plotlyOutput(ns("pta_output"), height = "620px"),
                uiOutput(ns("footer_pta"))
              ),
              tabPanel(
                title = "Probability interval",
                plotlyOutput(ns("pta_output_probability"), height = "620px"),
                uiOutput(ns("footer_pta_probability"))
              ),
              tabPanel(
                title = "CFR",
                plotlyOutput(ns("cfr_output"), height = "620px"),
                uiOutput(ns("footer_cfr"))
              )
            )
          )
        )
      ),
      column(
        width = 3,
        box(
          width = 12,
          title = tagList(icon("user-injured"), "Patient profile"),
          status = "warning",
          solidHeader = TRUE,
          class = "icu-card icu-card--controls",
          numericInput(ns("age"), label = labels("age", "label", language), value = 18, min = 0, max = 120, step = 1),
          numericInput(ns("height"), label = labels("height", "label", language), value = 180, min = 0, max = 250, step = 1),
          fluidRow(
            column(width = 8, numericInput(ns("weight"), label = labels("weight", "label", language), value = 70, min = 0, max = 1100, step = 1)),
            column(width = 4, selectInput(ns("weight_unit"), label = "Unit", choices = c("kg" = "kg", "lbs" = "lbs"), selected = "kg"))
          ),
          fluidRow(
            column(width = 8, numericInput(ns("creatinine"), label = labels("creatinine", "label", language), value = 60, min = 0, max = 1500, step = 1)),
            column(width = 4, selectInput(ns("creatinine_unit"), label = "Unit", choices = c("mg/dL" = "mg/dL", "umol/L" = "uM/L"), selected = "uM/L"))
          ),
          selectInput(ns("sex"), label = labels("sex", "label", language), choices = labels("sex", "choices", language), selected = "Male"),
          fluidRow(
            column(width = 6, numericInput(ns("urine_creatinine"), label = "Urinary creatinine (mmol/L)", value = 0, min = 0, max = 100)),
            column(width = 6, numericInput(ns("urine_output"), label = "Urine output (mL / 24 h)", value = 0, min = 0, max = 20000, step = 50))
          ),
          numericInput(ns("manual_renal_function"), label = "Manual renal function override (mL/min)", value = NA_real_, min = 0, step = 1)
          ,
          uiOutput(ns("renal_function_method"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    mic_information <- reactiveVal(NULL)
    mic_specie <- reactiveVal(NULL)
    ecoff <- reactiveVal(NA_real_)
    ecoff_ci <- reactiveVal(NULL)

    # create validator to prevent crashes when computing PTA with invalid inputs
    validator <- InputValidator$new()

    validator$add_rule("drug_dose", function(value) {if (value == 0) {"Dose must be greater than 0"}})
    validator$add_rule("height", function(value) {if (value < 10) {"Height must be in cm"}})
    validator$add_rule("height", function(value) {if (value > 250) {"Height must be less than 250 cm"}})
    validator$add_rule("weight", function(value) {if (value < 1) {"Weight must be greater than 0"}})
    validator$add_rule("age", function(value) {if (value <= 0) {"Age must be greater than 0"}})
    validator$add_rule("age", function(value) {if (value > 120) {"Age must be less than 120"}    })
    validator$add_rule("manual_renal_function", function(value) {if (!is.na(value) && value < 0) {"Manual renal function must be greater than or equal to 0"}})
    validator$add_rule("beta_lactamin", function(value) {if (is.null(value) || !nzchar(value)) {"Choose a drug before computing PTA"}})
    validator$add_rule("model_selected", function(value) {if (isTRUE(input$advanced_user_mode) && (is.null(value) || !nzchar(value))) {"Choose a model when advanced user mode is enabled"}})
    validator$add_rule("concentration_percentile", function(value) {if (isTRUE(input$advanced_user_mode) && (is.null(value) || !is.finite(value) || value <= 0 || value >= 1)) {"Concentration percentile must be between 0 and 1 when advanced user mode is enabled"}})

    validator$add_rule("weight", function(value) {
      max_weight <- if (identical(input$weight_unit, "lbs")) 1100 else 500

      if (value > max_weight) {
        paste0("Weight must be less than ", max_weight, if (identical(input$weight_unit, "lbs")) " lbs" else " kg")
      }
    })
    validator$enable()

    observeEvent(input$beta_lactamin, {
      model_choices <- if (is.null(input$beta_lactamin) || !nzchar(input$beta_lactamin) || !length(list_models_for_drug(input$beta_lactamin))) {
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

      updateSelectInput(
        session,
        "model_selected",
        choices = model_choices,
        selected = selected_model
      )
    }, ignoreInit = FALSE)

    eucast <- update_eucast()
    eucast_mic <- read_eucast_mic()
    updateSelectInput(
      session,
      "bacteria_select",
      choices = c("Probabilistic" = "probabilist", eucast[[2]]$bacteria)
    )

    output$footer_cfr <- renderUI({
      footer_note(
        if (length(eucast_mic)) {
          "Select a bacterium from EUCAST to compute the cumulative fraction of response."
        } else {
          "EUCAST MIC data are read from app/static/eucast.json and app/static/eucast_mic.json. Populate the MIC cache manually to enable CFR."
        }
      )
    })

    output$patient_summary <- renderUI({
      patient_summary_placeholder()
    })

    selected_model <- reactive({
      if (isTRUE(input$advanced_user_mode) && !is.null(input$model_selected) && nzchar(input$model_selected)) {
        return(input$model_selected)
      }

      if (is.null(input$beta_lactamin) || !nzchar(input$beta_lactamin)) {
        return(character(0))
      }

      get_default_model(input$beta_lactamin)
    })

    selected_model_definition <- reactive({
      if (!isTRUE(input$advanced_user_mode)) {
        return(NULL)
      }

      if (is.null(input$beta_lactamin) || !nzchar(input$beta_lactamin)) {
        return(NULL)
      }

      current_model <- selected_model()

      if (is.null(current_model) || !length(current_model) || !nzchar(current_model)) {
        return(NULL)
      }

      get_model_definition(drug = input$beta_lactamin, model = current_model)
    })

    output$model_selected_label <- renderUI({
      advanced_model_label(selected_model_definition())
    })

    output$model_selected_warning <- renderUI({
      advanced_model_warning_note(selected_model_definition())
    })

    output$renal_function_method <- renderUI({
      renal_formula_note(
        drug = input$beta_lactamin,
        model = selected_model(),
        manual_renal_function = input$manual_renal_function
      )
    })

    observeEvent(list(input$bacteria_select, input$beta_lactamin), {
      if (identical(input$bacteria_select, "probabilist") || is.null(input$beta_lactamin) || !nzchar(input$beta_lactamin)) {
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

    observeEvent(input$compute_pta, {
      if (!validator$is_valid()) {
        showNotification(
          "Please fix the highlighted inputs before continuing.",
          duration = 8,
          type = "error",
          closeButton = TRUE
        )
        return()
      }

      if (!identical(input$bacteria_select, "probabilist") && is.null(mic_information())) {
        showNotification(
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

      free_fraction_requested <- isTRUE(input$advanced_user_mode) && isTRUE(input$use_free_fraction)
      use_free_fraction <- free_fraction_requested &&
        is.finite(model_param$fu) &&
        model_param$fu > 0 &&
        model_param$fu <= 1
      concentration_multiplier <- if (use_free_fraction) model_param$fu else 1

      # ponytail: missing fu falls back to total Css so existing registry rows keep working; populate model_registry fu values to enable unbound exposure.
      if (free_fraction_requested && !use_free_fraction) {
        showNotification(
          "The selected model has no valid free fraction configured. Total Css was used.",
          duration = 8,
          type = "warning",
          closeButton = TRUE
        )
      }

      toxicity_threshold <- model_param$toxicity_threshold
      additional_concentration <- if (
        is.finite(input$additional_concentration) && input$additional_concentration > 0
      ) {
        input$additional_concentration
      } else {
        NA_real_
      }

      concentration_df <- sim_concentration(
        dose = input$drug_dose * 1000,
        tvcl = model_param$cl,
        eta_cl = model_param$eta_cl,
        quantile = c(0.025, 0.975),
        css_quantile = input$concentration_percentile,
        mic = if (identical(input$bacteria_select, "probabilist")) {
          NA
        } else {
          build_plot_mic_grid(mic_specie())
        },
        dose_increment = model_param$dose_increment * 1000,
        toxicity_threshold = toxicity_threshold,
        additional_threshold = additional_concentration,
        concentration_multiplier = concentration_multiplier
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
          cfr_df <- calculate_cfr_mulitple_doses(
            dose_increment = model_param$dose_increment * 1000,
            dose_max = model_param$max_dose * 1000,
            tvcl = model_param$cl,
            eta_cl = model_param$eta_cl,
            mic_distribution = mic_distribution_df,
            toxicity_threshold = toxicity_threshold,
            concentration_multiplier = concentration_multiplier
          )

          cfr_plot <- plot.cfr(cfr_df)
          output$cfr_output <- renderPlotly({
            cfr_plotly(cfr_plot)
          })

          output$footer_cfr <- renderUI({
            footer_note("The dashed reference lines highlight 10% and 90% CFR.")
          })
        } else {
          output$cfr_output <- renderPlotly({
            NULL
          })

          output$footer_cfr <- renderUI({
            footer_note("The selected bacterium does not expose a usable MIC distribution for CFR computation.")
          })
        }
      } else {
        output$cfr_output <- renderPlotly({
          NULL
        })

        output$footer_cfr <- renderUI({
          footer_note(
            "Select a bacterium from EUCAST to compute the cumulative fraction of response."
          )
        })
      }

      pta_plot <- plot.pta(
        concentration_df,
        ecoff = if (identical(input$bacteria_select, "probabilist")) {NA} else {ecoff()},
        selected_dose = input$drug_dose,
        dose_increment = model_param$dose_increment,
        use_free_fraction = use_free_fraction
      )

      output$pta_output <- renderPlotly({ pta_plotly(pta_plot$pta_multiple_doses, concentration_df, use_free_fraction) })
      output$pta_output_probability <- renderPlotly({ pta_plotly(pta_plot$pta_ci_plot, concentration_df, use_free_fraction) })

      output$footer_pta <- renderUI({
        all_dose <- c(-2, -1, 0, 1, 2) * model_param$dose_increment + input$drug_dose
        tagList(
          dose_badges(all_dose),
          toxicity_badge(toxicity_threshold * concentration_multiplier),
          concentration_badge(additional_concentration)
        )
      })

      output$footer_pta_probability <- renderUI({
        if (identical(input$bacteria_select, "probabilist")) {
          return(footer_note("Fixed 95% confidence intervals are shown across the default MIC range."))
        }

        tags$div(
          class = "icu-footer-metrics",
          tags$span(tags$b("Probability interval:"), "95%"),
          tags$span(tags$b("ECOFF:"), ecoff(), " mg/L"),
          tags$span(tags$b("Confidence interval:"), ecoff_ci())
        )
      })

      output$patient_summary <- renderUI({
        patient_summary_card(biological, model_param)
      })
    })
  })
}