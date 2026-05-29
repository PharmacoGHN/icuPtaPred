box::use(
  app/logic/model_registry
)

#' Compatibility wrapper for registry-backed model parameters.
#'
#' @param model Model identifier.
#' @param biological Named list of patient covariates.
#' @param drug Optional drug name used to scope the registry lookup.
#' @param manual_renal_function Optional manual renal-function override.
#'
#' @return A named list with clearance, variability, dose increment, and renal metadata.
#' @export
get_model_parameters <- function(model, biological, drug = NULL, manual_renal_function = NA_real_) {
  model_registry$get_model_parameters(
    model = model,
    biological = biological,
    drug = drug,
    manual_renal_function = manual_renal_function
  )
}

#' Compatibility wrapper for the registry-backed toxicity threshold.
#'
#' @param drug Drug name.
#' @param model Optional model identifier. When omitted, the default model is used.
#'
#' @return A numeric toxicity threshold in mg/L, or `NA` when no threshold is configured.
#' @export
drug_threshold <- function(drug, model = NULL) {
  model_registry$get_model_definition(drug = drug, model = model)$toxicity_threshold[[1]]
}

#' Compatibility wrapper for the registry-backed maximum daily dose.
#'
#' @param drug Drug name.
#' @param model Optional model identifier. When omitted, the default model is used.
#'
#' @return A numeric maximum daily dose in grams.
#' @export
max_dose <- function(drug, model = NULL) {
  model_registry$get_model_definition(drug = drug, model = model)$max_dose[[1]]
}

#' Compatibility wrapper for the registry default-model lookup.
#'
#' @param drug Drug name.
#'
#' @return The default model identifier for the selected drug.
#' @export
get_default_model <- function(drug) {
  model_registry$get_default_model(drug)
}
