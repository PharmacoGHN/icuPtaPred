# function
# add not_in
# add function that get ascii code for special character
# add function to create repeated boxes with defaut parameters "defaut_box"

#' @title date_time_format
#' @description
#' date_time_format is a small helper function whose goal is to
#' format a date and time input into a specific output (YYYY/MM/DD HH:MM)
#'
#' @param date can take any date value. Default value is Sys.Date()
#' @param time can take any time value. Default value is Sys.time()
#'
#' @noRd

date_time_format <- function(date = Sys.Date(),
                             time = Sys.time()) {
  output <- paste(format(date, "%Y/%m/%d"), format(time, "%H:%M"))
  return(output)
}

#' is_unique
#'
#' @description
#' This function will return TRUE if values of vector are unique
#'
#' @param vector correspond to the input vector
#'
#' @noRd

is_unique <- function(vector) {
  return(!any(duplicated(vector)))
}


#' labels
#'
#' @description wrapper to simply language changes in interface label
#'
#' @noRd

labels <- function(
  name = NULL,
  arg = c("choices", "box", "tab", "label"),
  language = c("fr", "en")
) {
  return(ui_data[[language]][[arg]][[name]])
}

#' getSdFromCV
#'
#' @description convert coefficient of variation to standard deviation in case of log normal distribution
#'
#' @noRd


get_sd_from_cv <- function(cv) {
  return(sqrt(log(cv^2 + 1)))
}

#' getCvFromSd
#'
#' @description convert standard deviation to coefficient of variation in case of log normal distribution
#'
#' @noRd

get_cv_from_sd <- function(sd) {
  return(sqrt(exp(sd^2) - 1))
}