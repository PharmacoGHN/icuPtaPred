
#' @title update_eucast
#' @description
#' update_eucast purpose is to extract the actual list of antibiotics and
#' species from Eucast website
#' @noRd

update_eucast <- function() {
  search_page <- rvest::read_html("https://mic.eucast.org/search/")

  atb <- search_page |>
    rvest::html_elements(css = "#search_antibiotic > option") # css code for antibiotics list
  df_atb <- data.frame(
    "antibiotics" = rvest::html_text(atb, trim = TRUE),
    "atb_values" = rvest::html_attr(atb, name = "value")
  ) |>
    dplyr::filter(.data$atb_values != "-1")

  bact <- search_page |>
    rvest::html_elements(css = "#search_species > option") # css code for species list
  df_bact <- data.frame(
    "bacteria" = rvest::html_text(bact, trim = TRUE),
    "bacteria_values" = rvest::html_attr(bact, name = "value")
  ) |>
    dplyr::filter(.data$bacteria_values != "-1")

  output <- list(df_atb, df_bact)

  return(output)
}


#' @title mic_distribution
#' @description
#' mic_distribution purpose is to extract the actual list of antibiotics and
#' species from Eucast website
#' @param antibiotic antibiotic for which MIC distribution is needed
#' @param bact bacteria for which MIC distribution is needed
#' @param eucast output of update_eucast function
#' @import dplyr
#' @noRd

mic_distribution <- function(antibiotic, bact, eucast) {
  # Extraction of antibiotic code in Eucast database
  atb_value <- eucast[[1]] |>
    dplyr::filter(.data$antibiotics == antibiotic) |>
    dplyr::select("atb_values") |>
    dplyr::pull()

  # Creation of url for extraction of MIC distribution
  base_url <- c(
    "https://mic.eucast.org/search/?search%5Bmethod%5D=mic&search%5Bantibiotic%5D=",
    "&search%5Bspecies%5D=-1&search%5Bdisk_content%5D=-1&search%5Blimit%5D=99"
  )
  atb_url <- paste0(base_url[1], atb_value, base_url[2])

  # Webscrapping of MIC distributions from Eucast database
  mic_page <- rvest::read_html(atb_url)
  df_mic <- mic_page |>
    rvest::html_element("#search-results-table") |>
    rvest::html_table()

  colnames(df_mic)[1] <- "bacteria"

  df_mic <- df_mic |>
    dplyr::filter(.data$Distributions != "Distributions") # removing empty lines in df

  # Extraction of MIC distribution, ECOFF (and confidence interval) for the specified specie
  df_mic_specific <- dplyr::filter(df_mic, .data$bacteria == bact)

  # Extracting the ECOFF and confidence interval
  ecoff <- df_mic_specific$`(T)ECOFF`
  ecoff_ci <- df_mic_specific$`Confidence interval`

  # Removing columns with no data
  mic_col_to_remove <- df_mic_specific[1, ] |>
    dplyr::select(-c("Distributions", "Observations", "(T)ECOFF", "Confidence interval")) |>
    dplyr::select(dplyr::where(~ . == 0)) |>
    colnames()

  mic_distribution <- df_mic_specific |>
    dplyr::select(-c("bacteria", "Distributions", "Observations", "(T)ECOFF", "Confidence interval")) |>
    dplyr::select(-dplyr::all_of(mic_col_to_remove))

  output <- list(
    mic_dataframe = df_mic_specific,
    mic_distribution = mic_distribution,
    ecoff = ecoff,
    ecoff_ci = ecoff_ci
  )

  return(output)
}
