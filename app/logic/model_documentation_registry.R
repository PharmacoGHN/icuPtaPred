box::use(
  jsonlite[fromJSON, toJSON]
)

documentation_columns <- c(
  "drug",
  "model",
  "Title",
  "Authors",
  "Year",
  "Journal",
  "DOI",
  "URL",
  "Abstract_Introduction",
  "Abstract_Methods",
  "Abstract_Results",
  "Abstract_Conclusions",
  "Abstract",
  "Clearance_Formula",
  "Model_Description",
  "Population_Studied"
)

empty_documentation_dataframe <- function() {
  data.frame(
    drug = character(0),
    model = character(0),
    Title = character(0),
    Authors = character(0),
    Year = character(0),
    Journal = character(0),
    DOI = character(0),
    URL = character(0),
    Abstract_Introduction = character(0),
    Abstract_Methods = character(0),
    Abstract_Results = character(0),
    Abstract_Conclusions = character(0),
    Abstract = character(0),
    Clearance_Formula = character(0),
    Model_Description = character(0),
    Population_Studied = character(0),
    stringsAsFactors = FALSE
  )
}

documentation_as_dataframe <- function(documentation) {
  if (is.data.frame(documentation)) {
    return(documentation)
  }

  if (is.list(documentation) && length(documentation)) {
    return(as.data.frame(documentation, stringsAsFactors = FALSE))
  }

  empty_documentation_dataframe()
}

coerce_documentation_dataframe <- function(documentation) {
  documentation <- documentation_as_dataframe(documentation)
  missing_columns <- setdiff(documentation_columns, colnames(documentation))
  if (length(missing_columns) > 0) {
    for (column in missing_columns) {
      documentation[[column]] <- ""
    }
  }

  documentation <- documentation[, documentation_columns, drop = FALSE]

  for (column in documentation_columns) {
    documentation[[column]] <- trimws(as.character(documentation[[column]]))
  }

  documentation
}

documentation_dir_path <- function() {
  normalizePath(file.path(getwd(), "app", "models", "documentation"), mustWork = FALSE)
}

documentation_key_part <- function(value) {
  value <- tolower(trimws(value))
  value <- gsub("[^a-z0-9]+", "-", value)
  gsub("(^-+|-+$)", "", value)
}

documentation_file_path <- function(drug, model) {
  normalizePath(
    file.path(
      documentation_dir_path(),
      paste0(documentation_key_part(drug), "__", documentation_key_part(model), ".json")
    ),
    mustWork = FALSE
  )
}

documentation_row_to_list <- function(documentation) {
  result <- lapply(documentation_columns, function(column) documentation[[column]][[1]])
  names(result) <- documentation_columns
  result
}

documentation_stub <- function(
  drug,
  model,
  clearance_formula = "",
  model_description = ""
) {
  data.frame(
    drug = drug,
    model = model,
    Title = model,
    Authors = "",
    Year = "",
    Journal = "",
    DOI = "",
    URL = "",
    Abstract_Introduction = "",
    Abstract_Methods = "",
    Abstract_Results = "",
    Abstract_Conclusions = "",
    Abstract = "",
    Clearance_Formula = clearance_formula,
    Model_Description = model_description,
    Population_Studied = "",
    stringsAsFactors = FALSE
  )
}

read_documentation_file <- function(path) {
  if (!file.exists(path)) {
    return(empty_documentation_dataframe())
  }

  json_text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  if (!nzchar(trimws(json_text))) {
    return(empty_documentation_dataframe())
  }

  coerce_documentation_dataframe(fromJSON(json_text, simplifyDataFrame = TRUE))
}

write_documentation_file <- function(path, documentation) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  documentation <- coerce_documentation_dataframe(documentation)
  toJSON(
    documentation_row_to_list(documentation),
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  ) |>
    writeLines(con = path)
}

#' Return model documentation from its JSON file.
#'
#' @param drug Drug name.
#' @param model Model identifier.
#' @param fallback_clearance_formula Clearance formula used when no JSON file exists yet.
#' @param fallback_model_description Model description used when no JSON file exists yet.
#'
#' @return A named list containing documentation fields for the selected model.
#' @export
get_model_documentation <- function(
  drug,
  model,
  fallback_clearance_formula = "",
  fallback_model_description = ""
) {
  path <- documentation_file_path(drug, model)

  if (file.exists(path)) {
    return(documentation_row_to_list(read_documentation_file(path)))
  }

  documentation_row_to_list(
    documentation_stub(
      drug,
      model,
      clearance_formula = fallback_clearance_formula,
      model_description = fallback_model_description
    )
  )
}

#' Ensure a documentation JSON file exists for a model.
#'
#' @param drug Drug name.
#' @param model Model identifier.
#' @param fallback_clearance_formula Clearance formula used when seeding a new JSON file.
#' @param fallback_model_description Model description used when seeding a new JSON file.
#'
#' @return The path to the documentation JSON file.
#' @export
ensure_model_documentation_file <- function(
  drug,
  model,
  fallback_clearance_formula = "",
  fallback_model_description = ""
) {
  path <- documentation_file_path(drug, model)

  if (file.exists(path)) {
    return(path)
  }

  documentation <- coerce_documentation_dataframe(
    documentation_stub(
      drug,
      model,
      clearance_formula = fallback_clearance_formula,
      model_description = fallback_model_description
    )
  )

  write_documentation_file(path, documentation)

  path
}

#' Save model documentation to its JSON file.
#'
#' @param drug Drug name.
#' @param model Model identifier.
#' @param Title Documentation title.
#' @param Authors Documentation authors.
#' @param Year Publication year.
#' @param Journal Journal name.
#' @param DOI DOI value.
#' @param URL Source URL.
#' @param Abstract_Introduction Abstract introduction text.
#' @param Abstract_Methods Abstract methods text.
#' @param Abstract_Results Abstract results text.
#' @param Abstract_Conclusions Abstract conclusions text.
#' @param Clearance_Formula Clearance formula markup.
#' @param Model_Description Model description text.
#' @param Population_Studied Population description text.
#' @param Abstract Legacy free-form abstract field.
#'
#' @return A named list containing the saved documentation values.
#' @export
save_model_documentation <- function(
  drug,
  model,
  Title,
  Authors,
  Year,
  Journal,
  DOI,
  URL,
  Abstract_Introduction,
  Abstract_Methods,
  Abstract_Results,
  Abstract_Conclusions,
  Clearance_Formula,
  Model_Description,
  Population_Studied,
  Abstract = ""
) {
  documentation <- data.frame(
    drug = drug,
    model = model,
    Title = Title,
    Authors = Authors,
    Year = Year,
    Journal = Journal,
    DOI = DOI,
    URL = URL,
    Abstract_Introduction = Abstract_Introduction,
    Abstract_Methods = Abstract_Methods,
    Abstract_Results = Abstract_Results,
    Abstract_Conclusions = Abstract_Conclusions,
    Abstract = Abstract,
    Clearance_Formula = Clearance_Formula,
    Model_Description = Model_Description,
    Population_Studied = Population_Studied,
    stringsAsFactors = FALSE
  )

  documentation <- coerce_documentation_dataframe(documentation)
  write_documentation_file(documentation_file_path(drug, model), documentation)

  documentation_row_to_list(documentation)
}

#' Delete a model documentation JSON file.
#'
#' @param drug Drug name.
#' @param model Model identifier.
#'
#' @return `TRUE` when a file was deleted, otherwise `FALSE`.
#' @export
delete_model_documentation <- function(drug, model) {
  path <- documentation_file_path(drug, model)

  if (!file.exists(path)) {
    return(FALSE)
  }

  file.remove(path)
}