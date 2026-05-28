box::use(
  shiny[in_devmode],
  utils[read.csv]
)

box::use(
  app/logic/model_documentation_registry[delete_model_documentation, ensure_model_documentation_file],
  app/logic/utils[get_cv_from_sd, get_sd_from_cv]
)

empty_registry <- function() {
  data.frame(
    drug = character(0),
    model = character(0),
    is_default = logical(0),
    dose_increment = numeric(0),
    renal_metric = character(0),
    renal_formula = character(0),
    clearance_expr = character(0),
    eta_cl_expr = character(0),
    stringsAsFactors = FALSE
  )
}

registry_columns <- c(
  "drug",
  "model",
  "is_default",
  "dose_increment",
  "renal_metric",
  "renal_formula",
  "clearance_expr",
  "eta_cl_expr"
)

coerce_registry <- function(registry) {
  missing_columns <- setdiff(registry_columns, colnames(registry))
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Model registry is missing columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  registry <- registry[, registry_columns, drop = FALSE]
  registry$drug <- trimws(registry$drug)
  registry$model <- trimws(registry$model)
  registry$is_default <- tolower(as.character(registry$is_default)) %in% c("true", "1", "yes")
  registry$dose_increment <- as.numeric(registry$dose_increment)
  registry$renal_metric <- trimws(registry$renal_metric)
  registry$renal_formula <- trimws(registry$renal_formula)
  registry$clearance_expr <- trimws(registry$clearance_expr)
  registry$eta_cl_expr <- trimws(registry$eta_cl_expr)

  registry$renal_metric[registry$renal_metric == ""] <- "none"
  registry$renal_formula[registry$renal_formula == ""] <- "No renal formula"

  registry
}

validate_registry_row <- function(registry_row) {
  if (!nrow(registry_row)) {
    stop("The model registry row is empty.")
  }

  if (!nzchar(registry_row$drug[[1]])) {
    stop("Drug is required.")
  }

  if (!nzchar(registry_row$model[[1]])) {
    stop("Model name is required.")
  }

  if (!is.finite(registry_row$dose_increment[[1]]) || registry_row$dose_increment[[1]] <= 0) {
    stop("Dose increment must be a positive numeric value.")
  }

  if (!nzchar(registry_row$clearance_expr[[1]])) {
    stop("Clearance expression is required.")
  }

  if (!nzchar(registry_row$eta_cl_expr[[1]])) {
    stop("Eta CL expression is required.")
  }

  parse(text = registry_row$clearance_expr[[1]])
  parse(text = registry_row$eta_cl_expr[[1]])

  invisible(registry_row)
}

base_registry_path <- function() {
  normalizePath(file.path(getwd(), "app", "models", "model-registry.json"), mustWork = FALSE)
}

legacy_registry_path <- function() {
  normalizePath(file.path(getwd(), "app", "models", "model-registry.csv"), mustWork = FALSE)
}

dev_registry_dir <- function() {
  normalizePath(file.path(getwd(), "app", "models", "dev"), mustWork = FALSE)
}

read_legacy_registry_csv_file <- function(path) {
  if (!file.exists(path)) {
    return(empty_registry())
  }

  coerce_registry(read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA")))
}

json_escape <- function(value) {
  value <- gsub("\\", "\\\\", value, fixed = TRUE)
  value <- gsub("\"", "\\\"", value, fixed = TRUE)
  value <- gsub("\r", "\\r", value, fixed = TRUE)
  value <- gsub("\n", "\\n", value, fixed = TRUE)
  value <- gsub("\t", "\\t", value, fixed = TRUE)

  value
}

format_registry_json_value <- function(value, column) {
  if (is.na(value) || !nzchar(as.character(value))) {
    return("null")
  }

  if (column == "is_default") {
    return(if (isTRUE(value)) "true" else "false")
  }

  if (column == "dose_increment") {
    return(format(as.numeric(value), scientific = FALSE, trim = TRUE))
  }

  paste0('"', json_escape(as.character(value)), '"')
}

parse_registry_json_array <- function(text) {
  position <- 1
  text_length <- nchar(text)

  char_at <- function(index) {
    substr(text, index, index)
  }

  skip_whitespace <- function() {
    while (position <= text_length && grepl("[[:space:]]", char_at(position))) {
      position <<- position + 1
    }
  }

  parse_json_string <- function() {
    buffer <- character(0)

    if (char_at(position) != "\"") {
      stop("Malformed model registry JSON file.")
    }

    position <<- position + 1

    while (position <= text_length) {
      current <- char_at(position)

      if (current == "\"") {
        position <<- position + 1
        return(paste(buffer, collapse = ""))
      }

      if (current == "\\") {
        position <<- position + 1

        if (position > text_length) {
          stop("Malformed model registry JSON file.")
        }

        escaped <- char_at(position)

        if (escaped == "\"") {
          buffer <- c(buffer, "\"")
        } else if (escaped == "\\") {
          buffer <- c(buffer, "\\")
        } else if (escaped == "/") {
          buffer <- c(buffer, "/")
        } else if (escaped == "b") {
          buffer <- c(buffer, "\b")
        } else if (escaped == "f") {
          buffer <- c(buffer, "\f")
        } else if (escaped == "n") {
          buffer <- c(buffer, "\n")
        } else if (escaped == "r") {
          buffer <- c(buffer, "\r")
        } else if (escaped == "t") {
          buffer <- c(buffer, "\t")
        } else if (escaped == "u") {
          unicode_digits <- substr(text, position + 1, position + 4)
          buffer <- c(buffer, intToUtf8(strtoi(unicode_digits, base = 16L)))
          position <<- position + 4
        } else {
          buffer <- c(buffer, escaped)
        }
      } else {
        buffer <- c(buffer, current)
      }

      position <<- position + 1
    }

    stop("Malformed model registry JSON file.")
  }

  parse_json_value <- function() {
    skip_whitespace()

    if (position > text_length) {
      stop("Malformed model registry JSON file.")
    }

    current <- char_at(position)
    remaining_text <- substr(text, position, text_length)

    if (current == "\"") {
      return(parse_json_string())
    }

    if (startsWith(remaining_text, "true")) {
      position <<- position + 4
      return("true")
    }

    if (startsWith(remaining_text, "false")) {
      position <<- position + 5
      return("false")
    }

    if (startsWith(remaining_text, "null")) {
      position <<- position + 4
      return("")
    }

    number_match <- regexpr("^-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?", remaining_text, perl = TRUE)

    if (number_match[[1]] == 1) {
      matched_number <- regmatches(remaining_text, number_match)
      position <<- position + nchar(matched_number)
      return(matched_number)
    }

    stop("Malformed model registry JSON file.")
  }

  parse_json_object <- function() {
    skip_whitespace()

    if (position > text_length || char_at(position) != "{") {
      stop("Malformed model registry JSON file.")
    }

    position <<- position + 1

    registry_row <- as.list(rep("", length(registry_columns)))
    names(registry_row) <- registry_columns

    repeat {
      skip_whitespace()

      if (position <= text_length && char_at(position) == "}") {
        position <<- position + 1
        break
      }

      key <- parse_json_string()
      skip_whitespace()

      if (position > text_length || char_at(position) != ":") {
        stop("Malformed model registry JSON file.")
      }

      position <<- position + 1
      value <- parse_json_value()

      if (key %in% registry_columns) {
        registry_row[[key]] <- value
      }

      skip_whitespace()

      if (position <= text_length && char_at(position) == ",") {
        position <<- position + 1
      } else if (position <= text_length && char_at(position) == "}") {
        position <<- position + 1
        break
      } else {
        stop("Malformed model registry JSON file.")
      }
    }

    as.data.frame(registry_row, stringsAsFactors = FALSE)
  }

  skip_whitespace()

  if (position > text_length || char_at(position) != "[") {
    stop("Malformed model registry JSON file.")
  }

  position <- position + 1
  rows <- list()

  repeat {
    skip_whitespace()

    if (position <= text_length && char_at(position) == "]") {
      position <- position + 1
      break
    }

    rows[[length(rows) + 1]] <- parse_json_object()
    skip_whitespace()

    if (position <= text_length && char_at(position) == ",") {
      position <- position + 1
    } else if (position <= text_length && char_at(position) == "]") {
      position <- position + 1
      break
    } else {
      stop("Malformed model registry JSON file.")
    }
  }

  if (!length(rows)) {
    return(empty_registry())
  }

  coerce_registry(do.call(rbind, rows))
}

read_registry_json_file <- function(path) {
  if (!file.exists(path)) {
    return(empty_registry())
  }

  parse_registry_json_array(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
}

read_registry_dcf_file <- function(path) {
  if (!file.exists(path)) {
    return(empty_registry())
  }

  coerce_registry(as.data.frame(read.dcf(path), stringsAsFactors = FALSE))
}

write_registry_csv_file <- function(path, registry) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(registry, path, row.names = FALSE, quote = TRUE)
}

write_registry_json_file <- function(path, registry) {
  registry <- coerce_registry(registry)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  json_rows <- lapply(seq_len(nrow(registry)), function(index) {
    row_lines <- vapply(
      registry_columns,
      function(column) {
        sprintf(
          '    "%s": %s',
          column,
          format_registry_json_value(registry[[column]][[index]], column)
        )
      },
      character(1)
    )

    if (length(row_lines) > 1) {
      row_lines[-length(row_lines)] <- paste0(row_lines[-length(row_lines)], ",")
    }

    c("  {", row_lines, "  }")
  })

  file_lines <- c("[")

  if (length(json_rows)) {
    for (index in seq_along(json_rows)) {
      current_row <- json_rows[[index]]
      if (index < length(json_rows)) {
        current_row[length(current_row)] <- paste0(current_row[length(current_row)], ",")
      }

      file_lines <- c(file_lines, current_row)
    }
  }

  file_lines <- c(file_lines, "]")
  writeLines(file_lines, path, useBytes = TRUE)
}

migrate_legacy_registry_file <- function() {
  legacy_path <- legacy_registry_path()
  json_path <- base_registry_path()

  if (!file.exists(legacy_path)) {
    return(NULL)
  }

  registry <- read_legacy_registry_csv_file(legacy_path)
  write_registry_json_file(json_path, registry)
  file.remove(legacy_path)

  registry
}

dedupe_registry <- function(registry) {
  if (!nrow(registry)) {
    return(registry)
  }

  registry$key <- paste(registry$drug, registry$model, sep = "::")
  registry <- registry[!duplicated(registry$key, fromLast = TRUE), , drop = FALSE]
  registry$key <- NULL
  registry[order(registry$drug, registry$model), , drop = FALSE]
}

#' @export
is_admin_mode <- function() {
  isTRUE(in_devmode())
}

#' @export
load_model_registry <- function(include_dev = is_admin_mode()) {
  registry <- if (file.exists(base_registry_path())) {
    read_registry_json_file(base_registry_path())
  } else {
    migrated_registry <- migrate_legacy_registry_file()
    if (is.null(migrated_registry)) empty_registry() else migrated_registry
  }

  if (include_dev && dir.exists(dev_registry_dir())) {
    dev_files <- list.files(dev_registry_dir(), pattern = "\\.dcf$", full.names = TRUE)

    if (length(dev_files)) {
      dev_registry <- do.call(
        rbind,
        lapply(dev_files, read_registry_dcf_file)
      )

      registry <- rbind(registry, dev_registry)
    }
  }

  dedupe_registry(registry)
}

#' @export
upsert_model_definition <- function(
  drug,
  model,
  is_default,
  dose_increment,
  renal_metric,
  renal_formula,
  clearance_expr,
  eta_cl_expr
) {
  if (!is_admin_mode()) {
    stop("Model registry updates are only available in dev mode.")
  }

  registry <- load_model_registry(include_dev = FALSE)
  registry_row <- data.frame(
    drug = drug,
    model = model,
    is_default = is_default,
    dose_increment = dose_increment,
    renal_metric = renal_metric,
    renal_formula = renal_formula,
    clearance_expr = clearance_expr,
    eta_cl_expr = eta_cl_expr,
    stringsAsFactors = FALSE
  )

  registry_row <- coerce_registry(registry_row)
  validate_registry_row(registry_row)

  existing_key <- paste(registry$drug, registry$model, sep = "::")
  new_key <- paste(registry_row$drug[[1]], registry_row$model[[1]], sep = "::")
  existed_before <- new_key %in% existing_key
  registry <- registry[existing_key != new_key, , drop = FALSE]

  if (isTRUE(registry_row$is_default[[1]])) {
    registry$is_default[registry$drug == registry_row$drug[[1]]] <- FALSE
  }

  registry <- rbind(registry, registry_row)
  registry <- dedupe_registry(registry)

  if (!any(registry$drug == registry_row$drug[[1]] & registry$is_default)) {
    first_index <- which(registry$drug == registry_row$drug[[1]])[1]
    registry$is_default[first_index] <- TRUE
  }

  write_registry_json_file(base_registry_path(), registry)

  if (!existed_before) {
    ensure_model_documentation_file(
      registry_row$drug[[1]],
      registry_row$model[[1]],
      fallback_clearance_formula = registry_row$clearance_expr[[1]],
      fallback_model_description = paste0(
        "Editable registry model for ",
        registry_row$drug[[1]],
        ". Dose increment: ",
        registry_row$dose_increment[[1]],
        " g."
      )
    )
  }

  registry
}

#' @export
remove_model_definition <- function(drug, model) {
  if (!is_admin_mode()) {
    stop("Model registry updates are only available in dev mode.")
  }

  registry <- load_model_registry(include_dev = FALSE)
  target_key <- paste(drug, model, sep = "::")
  existing_key <- paste(registry$drug, registry$model, sep = "::")

  if (!(target_key %in% existing_key)) {
    stop("The selected model is not present in the model registry.")
  }

  registry <- registry[existing_key != target_key, , drop = FALSE]

  if (any(registry$drug == drug) && !any(registry$drug == drug & registry$is_default)) {
    first_index <- which(registry$drug == drug)[1]
    registry$is_default[first_index] <- TRUE
  }

  write_registry_json_file(base_registry_path(), registry)
  delete_model_documentation(drug, model)

  registry
}

#' @export
list_models_for_drug <- function(drug, include_dev = is_admin_mode()) {
  registry <- load_model_registry(include_dev)
  unique(registry$model[registry$drug == drug])
}

#' @export
get_model_definition <- function(drug = NULL, model = NULL, include_dev = is_admin_mode()) {
  registry <- load_model_registry(include_dev)

  if (!is.null(drug) && nzchar(drug)) {
    registry <- registry[registry$drug == drug, , drop = FALSE]
  }

  if (!nrow(registry)) {
    stop("No model definitions are available for the selected drug.")
  }

  if (is.null(model) || !nzchar(model)) {
    default_rows <- registry[registry$is_default, , drop = FALSE]

    if (nrow(default_rows)) {
      return(default_rows[1, , drop = FALSE])
    }

    return(registry[1, , drop = FALSE])
  }

  selected_rows <- registry[registry$model == model, , drop = FALSE]
  if (!nrow(selected_rows)) {
    stop("The selected model is not present in the model registry.")
  }

  selected_rows[1, , drop = FALSE]
}

#' @export
evaluate_model_expression <- function(expr, biological, renal_value = NA_real_) {
  evaluation_env <- list2env(
    c(
      as.list(biological),
      list(
        biological = biological,
        renal_value = renal_value,
        get_cv_from_sd = get_cv_from_sd,
        get_sd_from_cv = get_sd_from_cv
      )
    ),
    parent = baseenv()
  )

  eval(parse(text = expr), envir = evaluation_env)
}