box::use(
  bs4Dash[box, tabBox],
  shiny[
    tags, fluidRow, column, selectInput, textInput, numericInput, checkboxInput, textAreaInput, actionButton, updateSelectInput,
    updateTextInput, updateNumericInput, updateCheckboxInput, updateTextAreaInput, renderText, renderUI, withMathJax,
    showNotification, NS, moduleServer, tagList, HTML, icon, in_devmode, div, tabPanel, textOutput, uiOutput, reactiveVal,
    reactive, observeEvent, req, tag
  ],
  stats[setNames]
)

box::use(
  app/logic/model_documentation_registry[get_model_documentation, save_model_documentation],
  app/logic/model_registry[get_model_definition, list_models_for_drug, remove_model_definition, upsert_model_definition],
  app/logic/utils[labels]
)

renal_metric_choices <- c(
  "No renal formula" = "none",
  "Cockcroft-Gault (TBW)" = "cg_tbw",
  "Cockcroft-Gault (AJBW)" = "cg_ajbw",
  "Cockcroft-Gault (IBW)" = "cg_ibw",
  "Cockcroft-Gault (LBW)" = "cg_lbw",
  "MDRD" = "mdrd",
  "CKD-EPI 2009" = "ckd_2009",
  "CKD-EPI 2021" = "ckd_2021",
  "Schwartz" = "schwartz",
  "UV/P clearance" = "uvp",
  "EKFC" = "ekfc"
)

renal_formula_from_metric <- function(metric) {
  label <- names(renal_metric_choices[renal_metric_choices == metric])

  if (!length(label)) {
    return("No renal formula")
  }

  label[[1]]
}

format_numeric_literal <- function(value) {
  if (!is.finite(value)) {
    return("")
  }

  format(value, scientific = FALSE, trim = TRUE, digits = 8)
}

eta_expression_from_inputs <- function(value, is_cv) {
  numeric_literal <- format_numeric_literal(value)

  if (!nzchar(numeric_literal)) {
    return("")
  }

  if (isTRUE(is_cv)) {
    return(paste0("get_sd_from_cv(", numeric_literal, ")"))
  }

  numeric_literal
}

parse_eta_expression <- function(expr) {
  expr <- trimws(expr)

  cv_match <- regexec("^get_sd_from_cv\\((.+)\\)$", expr)
  cv_parts <- regmatches(expr, cv_match)[[1]]
  if (length(cv_parts) == 2) {
    return(list(value = as.numeric(cv_parts[[2]]), is_cv = TRUE))
  }

  sd_match <- regexec("^get_cv_from_sd\\((.+)\\)$", expr)
  sd_parts <- regmatches(expr, sd_match)[[1]]
  if (length(sd_parts) == 2) {
    return(list(value = as.numeric(sd_parts[[2]]), is_cv = FALSE))
  }

  list(value = as.numeric(expr), is_cv = FALSE)
}

abstract_section_fields <- c(
  "Introduction" = "Abstract_Introduction",
  "Methods" = "Abstract_Methods",
  "Results" = "Abstract_Results",
  "Conclusions" = "Abstract_Conclusions"
)

strip_legacy_abstract_markup <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return("")
  }

  text <- gsub("(?i)<br\\s*/?>", "\n", text, perl = TRUE)
  text <- gsub("(?i)</p>", "\n\n", text, perl = TRUE)
  text <- gsub("<[^>]+>", " ", text)
  text <- gsub("&nbsp;", " ", text, fixed = TRUE)
  text <- gsub("&amp;", "&", text, fixed = TRUE)
  text <- gsub("[ \t]+", " ", text)
  text <- gsub(" *\n *", "\n", text)
  text <- gsub("\n{3,}", "\n\n", text)

  trimws(text)
}

documentation_for_editor <- function(documentation) {
  has_structured_sections <- any(
    vapply(
      unname(abstract_section_fields),
      function(field) {
        !is.null(documentation[[field]]) && nzchar(trimws(documentation[[field]]))
      },
      logical(1)
    )
  )

  if (!has_structured_sections && !is.null(documentation$Abstract) && nzchar(trimws(documentation$Abstract))) {
    documentation$Abstract_Introduction <- strip_legacy_abstract_markup(documentation$Abstract)
  }

  documentation
}

render_plain_text_block <- function(text) {
  tags$div(class = "icu-prose icu-prose--plaintext", text)
}

render_abstract_ui <- function(documentation) {
  structured_sections <- Filter(
    Negate(is.null),
    lapply(names(abstract_section_fields), function(section_title) {
      field <- abstract_section_fields[[section_title]]
      text <- documentation[[field]]

      if (is.null(text) || !nzchar(trimws(text))) {
        return(NULL)
      }

      tags$div(
        class = "icu-abstract-section",
        tags$h5(section_title),
        render_plain_text_block(text)
      )
    })
  )

  if (length(structured_sections)) {
    return(tags$div(class = "icu-abstract-sections", structured_sections))
  }

  if (!is.null(documentation$Abstract) && nzchar(trimws(documentation$Abstract))) {
    return(div(class = "icu-prose", HTML(documentation$Abstract)))
  }

  tags$p("No abstract documented yet.", class = "icu-copy-block")
}

registry_editor <- function(ns) {
  if (!isTRUE(in_devmode())) {
    return(NULL)
  }

  box(
    width = 12,
    title = div(icon("pen-to-square"), "Registry editor", style = "display: flex; align-items: center; gap: 0.5rem; color: #17a2b8;"),
    status = "warning",
    solidHeader = TRUE,
    class = "icu-card icu-card--controls",
    tags$p(
      "Local dev only. Saving a new model writes to the registry JSON file and creates a documentation JSON file automatically.",
      class = "icu-copy-block"
    ),
    fluidRow(
      column(
        width = 6,
        selectInput(
          ns("edit_drug"),
          "Drug",
          choices = labels("drug", "choices", "fr"),
          selectize = FALSE
        )
      ),
      column(
        width = 6,
        selectInput(
          ns("edit_model_lookup"),
          "Registry model",
          choices = c("Create new model" = ""),
          selectize = FALSE
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        textInput(ns("edit_model"), "Model name")
      ),
      column(
        width = 6,
        checkboxInput(ns("edit_is_default"), "Default model", value = FALSE)
      )
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(ns("edit_dose_increment"), "Dose increment (g)", value = 1, min = 0.125, step = 0.125)
      ),
      column(
        width = 6,
        selectInput(
          ns("edit_renal_metric"),
          "Renal metric",
          choices = renal_metric_choices,
          selectize = FALSE
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(ns("edit_max_dose"), "Max daily dose (g)", value = 20, min = 0.125, step = 0.125)
      ),
      column(
        width = 6,
        numericInput(ns("edit_toxicity_threshold"), "Toxicity threshold (mg/L)", value = NA_real_, min = 0, step = 0.1)
      )
    ),
    tags$div(
      class = "icu-inline-note",
      tags$strong("Renal formula label: "),
      textOutput(ns("edit_renal_formula_label"), container = tags$span)
    ),
    fluidRow(
      column(
        width = 8,
        textAreaInput(ns("edit_clearance_expr"), "Clearance expression", rows = 4)
      ),
      column(
        width = 4,
        numericInput(ns("edit_eta_cl_value"), "Eta CL value", value = 1, min = 0, step = 0.001),
        checkboxInput(ns("edit_eta_is_cv"), "Interpret eta CL value as CV%", value = FALSE),
        tags$div(
          class = "icu-inline-note",
          tags$strong("Generated eta CL expression: "),
          textOutput(ns("edit_eta_cl_expr_preview"), container = tags$span)
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        actionButton(ns("load_selected_model"), "Load selected model", class = "btn-default"),
        tags$span(style = "display:inline-block; width: 0.5rem;"),
        actionButton(ns("new_model"), "New model", class = "btn-default"),
        tags$span(style = "display:inline-block; width: 0.5rem;"),
        actionButton(ns("save_model"), "Save model", class = "btn-warning"),
        tags$span(style = "display:inline-block; width: 0.5rem;"),
        actionButton(ns("delete_model"), "Delete model", class = "btn-danger")
      )
    )
  )
}

documentation_editor <- function(ns) {
  if (!isTRUE(in_devmode())) {
    return(NULL)
  }

  box(
    width = 12,
    title = tagList(icon("file-lines"), "Documentation editor"),
    status = "info",
    solidHeader = TRUE,
    class = "icu-card icu-card--controls",
    tags$p(
      "Local dev only. This editor writes a model-specific documentation JSON file used by the model library.",
      class = "icu-copy-block"
    ),
    uiOutput(ns("documentation_target")),
    fluidRow(
      column(
        width = 5,
        textInput(ns("doc_title"), "Title"),
        textInput(ns("doc_authors"), "Authors"),
        fluidRow(
          column(
            width = 4,
            textInput(ns("doc_year"), "Year")
          ),
          column(
            width = 4,
            textInput(ns("doc_journal"), "Journal")
          ),
          column(
            width = 4,
            textInput(ns("doc_doi"), "DOI")
          )
        ),
        textInput(ns("doc_url"), "URL"),
        textAreaInput(ns("doc_population_studied"), "Population studied", rows = 4)
      ),
      column(
        width = 7,
        textAreaInput(ns("doc_model_description"), "Model description", rows = 5),
        textAreaInput(ns("doc_clearance_formula"), "Displayed clearance formula", rows = 4),
        fluidRow(
          column(
            width = 6,
            textAreaInput(ns("doc_abstract_introduction"), "Abstract - introduction", rows = 5)
          ),
          column(
            width = 6,
            textAreaInput(ns("doc_abstract_methods"), "Abstract - methods", rows = 5)
          )
        ),
        fluidRow(
          column(
            width = 6,
            textAreaInput(ns("doc_abstract_results"), "Abstract - results", rows = 5)
          ),
          column(
            width = 6,
            textAreaInput(ns("doc_abstract_conclusions"), "Abstract - conclusions", rows = 5)
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        actionButton(ns("reload_selected_documentation"), "Reload selected documentation", class = "btn-default"),
        tags$span(style = "display:inline-block; width: 0.5rem;"),
        actionButton(ns("save_documentation"), "Save documentation", class = "btn-info")
      )
    )
  )
}

doi_link <- function(model) {
  has_link <- !is.null(model$URL) && !is.na(model$URL) && nzchar(model$URL) && grepl("^https?://", model$URL)
  has_doi <- !is.null(model$DOI) && !is.na(model$DOI) && nzchar(model$DOI)

  if (!has_doi) {
    return(NULL)
  }

  if (!has_link) {
    return(tags$span(model$DOI))
  }

  tags$a(model$DOI, href = model$URL, target = "_blank")
}

model_overview_card <- function(ns) {
  box(
    width = 12,
    title = textOutput(ns("title")),
    status = "success",
    solidHeader = TRUE,
    class = "icu-card icu-card--article",
    div(
      class = "icu-detail-block",
      tags$h4("Citation"),
      uiOutput(ns("authors"))
    ),
    div(
      class = "icu-detail-block",
      tags$h4("Abstract"),
      uiOutput(ns("abstract"))
    ),
    div(
      class = "icu-detail-grid",
      div(
        class = "icu-detail-block",
        tags$h4("Clearance formula"),
        uiOutput(ns("clearance_formula"))
      ),
      div(
        class = "icu-detail-block",
        tags$h4("Model description"),
        textOutput(ns("model_description"))
      ),
      div(
        class = "icu-detail-block",
        tags$h4("Population studied"),
        textOutput(ns("population_studied"))
      )
    )
  )
}

library_tabs <- function(ns) {
  if (isTRUE(in_devmode())) {
    return(
      div(
        class = "icu-library-tabs",
        tabBox(
          width = 12,
          selected = "Model overview",
          tabPanel("Model overview", model_overview_card(ns)),
          tabPanel("Registry editor", registry_editor(ns)),
          tabPanel("Documentation editor", documentation_editor(ns))
        )
      )
    )
  }

  div(
    class = "icu-library-tabs",
    tabBox(
      width = 12,
      selected = "Model overview",
      tabPanel("Model overview", model_overview_card(ns))
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        box(
          width = 12,
          title = tagList(icon("filter"), "Filters"),
          status = "primary",
          solidHeader = TRUE,
          class = "icu-card icu-card--controls",
          selectInput(
            ns("drug"),
            "Drug",
            choices = labels("drug", "choices", "fr"),
            selectize = FALSE
          ),
          selectInput(
            ns("model"),
            "Model",
            choices = "No model currently available",
            selectize = FALSE
          )
        )
      ),
      column(
        width = 9,
        library_tabs(ns)
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    refresh_token <- reactiveVal(0)

    available_models <- reactive({
      refresh_token()

      if (is.null(input$drug) || !nzchar(input$drug)) {
        return(character(0))
      }

      list_models_for_drug(input$drug)
    })

    selected_model_name <- reactive({
      models <- available_models()

      if (!length(models)) {
        return(NULL)
      }

      if (!is.null(input$model) && nzchar(input$model) && input$model %in% models) {
        return(input$model)
      }

      models[[1]]
    })

    selected_registry_definition <- reactive({
      req(input$drug)

      current_model <- selected_model_name()
      req(!is.null(current_model), nzchar(current_model))

      get_model_definition(drug = input$drug, model = current_model)
    })

    selected_documentation <- reactive({
      registry_definition <- selected_registry_definition()

      get_model_documentation(
        drug = registry_definition$drug[[1]],
        model = registry_definition$model[[1]],
        fallback_clearance_formula = registry_definition$clearance_expr[[1]],
        fallback_model_description = paste0(
          "Editable registry model for ",
          registry_definition$drug[[1]],
          ". Dose increment: ",
          registry_definition$dose_increment[[1]],
          " g."
        )
      )
    })

    observeEvent(list(input$drug, available_models()), {
      if (is.null(input$drug) || !nzchar(input$drug)) {
        updateSelectInput(session, "model", choices = character(0), selected = character(0))
        return()
      }

      models <- available_models()
      selected_model <- selected_model_name()

      updateSelectInput(
        session,
        "model",
        choices = models,
        selected = if (is.null(selected_model)) character(0) else selected_model
      )
    }, ignoreInit = FALSE)

    output$title <- renderText({
      selected_documentation()$Title
    })

    output$authors <- renderUI({
      documentation <- selected_documentation()
      citation <- paste0(documentation$Authors, " ", documentation$Journal, ", ", documentation$Year, ". ")

      tagList(citation, "DOI: ", doi_link(documentation))
    })

    output$abstract <- renderUI({
      render_abstract_ui(selected_documentation())
    })

    output$clearance_formula <- renderUI({
      withMathJax(HTML(selected_documentation()$Clearance_Formula))
    })

    output$model_description <- renderText({
      selected_documentation()$Model_Description
    })

    output$population_studied <- renderText({
      selected_documentation()$Population_Studied
    })

    if (isTRUE(in_devmode())) {
      output$edit_renal_formula_label <- renderText({
        renal_formula_from_metric(input$edit_renal_metric)
      })

      output$edit_eta_cl_expr_preview <- renderText({
        eta_expression_from_inputs(input$edit_eta_cl_value, input$edit_eta_is_cv)
      })

      output$documentation_target <- renderUI({
        current_definition <- selected_registry_definition()

        if (!nrow(current_definition)) {
          return(
            tags$p(
              "Select a drug and model from the filters above to edit documentation.",
              class = "icu-copy-block"
            )
          )
        }

        tags$div(
          class = "icu-inline-note",
          tags$strong("Current documentation target: "),
          paste(current_definition$drug[[1]], "-", current_definition$model[[1]])
        )
      })

      load_documentation_inputs <- function() {
        current_model <- selected_model_name()

        if (is.null(input$drug) || !nzchar(input$drug) || is.null(current_model) || !nzchar(current_model)) {
          updateTextInput(session, "doc_title", value = "")
          updateTextInput(session, "doc_authors", value = "")
          updateTextInput(session, "doc_year", value = "")
          updateTextInput(session, "doc_journal", value = "")
          updateTextInput(session, "doc_doi", value = "")
          updateTextInput(session, "doc_url", value = "")
          updateTextAreaInput(session, "doc_clearance_formula", value = "")
          updateTextAreaInput(session, "doc_model_description", value = "")
          updateTextAreaInput(session, "doc_population_studied", value = "")
          updateTextAreaInput(session, "doc_abstract_introduction", value = "")
          updateTextAreaInput(session, "doc_abstract_methods", value = "")
          updateTextAreaInput(session, "doc_abstract_results", value = "")
          updateTextAreaInput(session, "doc_abstract_conclusions", value = "")
          return()
        }

        documentation <- documentation_for_editor(selected_documentation())
        updateTextInput(session, "doc_title", value = documentation$Title)
        updateTextInput(session, "doc_authors", value = documentation$Authors)
        updateTextInput(session, "doc_year", value = documentation$Year)
        updateTextInput(session, "doc_journal", value = documentation$Journal)
        updateTextInput(session, "doc_doi", value = documentation$DOI)
        updateTextInput(session, "doc_url", value = documentation$URL)
        updateTextAreaInput(session, "doc_clearance_formula", value = documentation$Clearance_Formula)
        updateTextAreaInput(session, "doc_model_description", value = documentation$Model_Description)
        updateTextAreaInput(session, "doc_population_studied", value = documentation$Population_Studied)
        updateTextAreaInput(session, "doc_abstract_introduction", value = documentation$Abstract_Introduction)
        updateTextAreaInput(session, "doc_abstract_methods", value = documentation$Abstract_Methods)
        updateTextAreaInput(session, "doc_abstract_results", value = documentation$Abstract_Results)
        updateTextAreaInput(session, "doc_abstract_conclusions", value = documentation$Abstract_Conclusions)
      }

      observeEvent(list(input$drug, input$model, refresh_token()), {
        load_documentation_inputs()
      }, ignoreInit = FALSE)

      observeEvent(input$edit_drug, {
        edit_models <- if (is.null(input$edit_drug) || !nzchar(input$edit_drug)) {
          character(0)
        } else {
          list_models_for_drug(input$edit_drug)
        }

        updateSelectInput(
          session,
          "edit_model_lookup",
          choices = c("Create new model" = "", setNames(edit_models, edit_models)),
          selected = ""
        )
      }, ignoreInit = FALSE)

      reset_registry_editor <- function(drug = input$drug) {
        updateSelectInput(
          session,
          "edit_drug",
          selected = if (!is.null(drug) && nzchar(drug)) drug else NULL
        )
        updateSelectInput(session, "edit_model_lookup", selected = "")
        updateTextInput(session, "edit_model", value = "")
        updateCheckboxInput(session, "edit_is_default", value = FALSE)
        updateNumericInput(session, "edit_dose_increment", value = 1)
        updateNumericInput(session, "edit_max_dose", value = 20)
        updateNumericInput(session, "edit_toxicity_threshold", value = NA_real_)
        updateSelectInput(session, "edit_renal_metric", selected = "none")
        updateNumericInput(session, "edit_eta_cl_value", value = 1)
        updateCheckboxInput(session, "edit_eta_is_cv", value = FALSE)
        updateTextAreaInput(session, "edit_clearance_expr", value = "")
      }

      observeEvent(input$load_selected_model, {
        if (is.null(input$edit_drug) || !nzchar(input$edit_drug)) {
          return()
        }

        if (is.null(input$edit_model_lookup) || !nzchar(input$edit_model_lookup)) {
          reset_registry_editor(drug = input$edit_drug)
          return()
        }

        definition <- get_model_definition(drug = input$edit_drug, model = input$edit_model_lookup)
        eta_input <- parse_eta_expression(definition$eta_cl_expr[[1]])

        updateSelectInput(session, "edit_drug", selected = definition$drug[[1]])
        updateSelectInput(session, "edit_model_lookup", selected = definition$model[[1]])
        updateTextInput(session, "edit_model", value = definition$model[[1]])
        updateCheckboxInput(session, "edit_is_default", value = definition$is_default[[1]])
        updateNumericInput(session, "edit_dose_increment", value = definition$dose_increment[[1]])
        updateNumericInput(session, "edit_max_dose", value = definition$max_dose[[1]])
        updateNumericInput(session, "edit_toxicity_threshold", value = definition$toxicity_threshold[[1]])
        updateSelectInput(session, "edit_renal_metric", selected = definition$renal_metric[[1]])
        updateTextAreaInput(session, "edit_clearance_expr", value = definition$clearance_expr[[1]])
        updateNumericInput(session, "edit_eta_cl_value", value = eta_input$value)
        updateCheckboxInput(session, "edit_eta_is_cv", value = eta_input$is_cv)
      })

      observeEvent(input$new_model, {
        reset_registry_editor()
      })

      observeEvent(input$save_model, {
        eta_expression <- eta_expression_from_inputs(input$edit_eta_cl_value, input$edit_eta_is_cv)

        registry <- upsert_model_definition(
          drug = input$edit_drug,
          model = input$edit_model,
          is_default = input$edit_is_default,
          dose_increment = input$edit_dose_increment,
          max_dose = input$edit_max_dose,
          toxicity_threshold = input$edit_toxicity_threshold,
          renal_metric = input$edit_renal_metric,
          renal_formula = renal_formula_from_metric(input$edit_renal_metric),
          clearance_expr = input$edit_clearance_expr,
          eta_cl_expr = eta_expression
        )

        updated_models <- unique(registry$model[registry$drug == input$edit_drug])
        updateSelectInput(
          session,
          "edit_model_lookup",
          choices = c("Create new model" = "", setNames(updated_models, updated_models)),
          selected = input$edit_model
        )
        updateSelectInput(session, "drug", selected = input$edit_drug)
        updateSelectInput(session, "model", choices = updated_models, selected = input$edit_model)
        refresh_token(refresh_token() + 1)

        showNotification(
           paste0("Saved model ", input$edit_model, " to the registry JSON file and ensured its documentation JSON file exists."),
          type = "message",
          duration = 6
        )
      })

      observeEvent(input$delete_model, {
        if (is.null(input$edit_drug) || !nzchar(input$edit_drug) || is.null(input$edit_model_lookup) || !nzchar(input$edit_model_lookup)) {
          showNotification(
            "Load an existing model from the registry editor before deleting it.",
            type = "error",
            duration = 6
          )
          return()
        }

        deleted_model <- input$edit_model_lookup
        registry <- remove_model_definition(input$edit_drug, deleted_model)
        updated_models <- unique(registry$model[registry$drug == input$edit_drug])
        next_model <- if (length(updated_models)) updated_models[[1]] else character(0)

        updateSelectInput(
          session,
          "edit_model_lookup",
          choices = c("Create new model" = "", setNames(updated_models, updated_models)),
          selected = ""
        )
        updateSelectInput(session, "drug", selected = input$edit_drug)
        updateSelectInput(session, "model", choices = updated_models, selected = next_model)
        reset_registry_editor(drug = input$edit_drug)
        refresh_token(refresh_token() + 1)

        showNotification(
           paste0("Removed model ", deleted_model, " from the registry and deleted its documentation JSON file."),
          type = "warning",
          duration = 6
        )
      })

      observeEvent(input$reload_selected_documentation, {
        load_documentation_inputs()
      })

      observeEvent(input$save_documentation, {
        current_definition <- selected_registry_definition()

        if (!nrow(current_definition)) {
          showNotification(
            "Select a drug and model from the filters before saving documentation.",
            type = "error",
            duration = 6
          )
          return()
        }

        save_model_documentation(
          drug = current_definition$drug[[1]],
          model = current_definition$model[[1]],
          Title = input$doc_title,
          Authors = input$doc_authors,
          Year = input$doc_year,
          Journal = input$doc_journal,
          DOI = input$doc_doi,
          URL = input$doc_url,
          Abstract_Introduction = input$doc_abstract_introduction,
          Abstract_Methods = input$doc_abstract_methods,
          Abstract_Results = input$doc_abstract_results,
          Abstract_Conclusions = input$doc_abstract_conclusions,
          Clearance_Formula = input$doc_clearance_formula,
          Model_Description = input$doc_model_description,
          Population_Studied = input$doc_population_studied
        )

        refresh_token(refresh_token() + 1)

        showNotification(
          paste0("Saved documentation for ", current_definition$model[[1]], "."),
          type = "message",
          duration = 6
        )
      })
    }
  })
}