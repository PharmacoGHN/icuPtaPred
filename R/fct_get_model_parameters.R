#' get_model_parameters
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

get_model_parameters <- function(model, biological) {

  cl <- dplyr::case_when(
    # Piperacillin
    model == "klastrup_2020" ~ (2.25 + 0.119 * biological$cg_tbw),
    # Cefepim
    model == "an_2023" ~ 4, #(2 * biological$cg_lbw / 54 + 0.526), # simplified only patient without CRRT
    .default = 0
  )

  eta_cl <- dplyr::case_when(
    #Piperacillin
    model == "klastrup_2020" ~ 1,
    .default = 0
  )
  return(list(cl = cl, eta_cl = eta_cl))
}
