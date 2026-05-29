box::use(
  jsonlite[toJSON, fromJSON],
  utils[read.csv]
)

box::use(
  app/logic/helper_model_information[model_information]
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

coerce_documentation_dataframe <- function(documentation) {
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
      paste0(documentation_key_part(drug), "__", documentation_key_part(model), ".csv")
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

static_documentation <- function(drug, model) {
  if (
    is.null(model_information[[drug]]) ||
    is.null(model_information[[drug]][[model]])
  ) {
    return(NULL)
  }

  entry <- model_information[[drug]][[model]]

  data.frame(
    drug = drug,
    model = model,
    Title = if (!is.null(entry$Title)) as.character(entry$Title[[1]]) else model,
    Authors = if (!is.null(entry$Authors)) as.character(entry$Authors[[1]]) else "",
    Year = if (!is.null(entry$Year)) as.character(entry$Year[[1]]) else "",
    Journal = if (!is.null(entry$Journal)) as.character(entry$Journal[[1]]) else "",
    DOI = if (!is.null(entry$DOI) && !is.na(entry$DOI[[1]])) as.character(entry$DOI[[1]]) else "",
    URL = if (!is.null(entry$URL) && !is.na(entry$URL[[1]])) as.character(entry$URL[[1]]) else "",
    Abstract_Introduction = "",
    Abstract_Methods = "",
    Abstract_Results = "",
    Abstract_Conclusions = "",
    Abstract = if (!is.null(entry$Abstract)) as.character(entry$Abstract[[1]]) else "",
    Clearance_Formula = if (!is.null(entry$Clearance_Formula)) as.character(entry$Clearance_Formula[[1]]) else "",
    Model_Description = if (!is.null(entry$Model_Description)) as.character(entry$Model_Description[[1]]) else "",
    Population_Studied = if (!is.null(entry$Population_Studied)) as.character(entry$Population_Studied[[1]]) else "",
    stringsAsFactors = FALSE
  )
}

read_documentation_file <- function(path) {
  if (!file.exists(path)) {
    return(empty_documentation_dataframe())
  }

  coerce_documentation_dataframe(read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA")))
}

write_documentation_file <- function(path, documentation) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  toJSON(documentation, pretty = TRUE) |>
    writeLines(con = path)
}

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

  static_doc <- static_documentation(drug, model)
  if (!is.null(static_doc)) {
    return(documentation_row_to_list(coerce_documentation_dataframe(static_doc)))
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

  static_doc <- static_documentation(drug, model)
  documentation <- if (!is.null(static_doc)) {
    coerce_documentation_dataframe(static_doc)
  } else {
    coerce_documentation_dataframe(
      documentation_stub(
        drug,
        model,
        clearance_formula = fallback_clearance_formula,
        model_description = fallback_model_description
      )
    )
  }

  write_documentation_file(path, documentation)

  path
}

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

#' @export
delete_model_documentation <- function(drug, model) {
  path <- documentation_file_path(drug, model)

  if (!file.exists(path)) {
    return(FALSE)
  }

  file.remove(path)
}