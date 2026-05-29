box::use(
  dplyr[all_of, filter, pull, select, where],
  rvest[html_attr, html_element, html_elements, html_table, html_text, read_html]
)

#' Download the current EUCAST antibiotic and species lists.
#'
#' @return A list with the antibiotic lookup table and the species lookup table.
#' @export
update_eucast <- function() {
  search_page <- read_html("https://mic.eucast.org/search/")

  atb <- search_page |>
    html_elements(css = "#search_antibiotic > option")
  df_atb <- data.frame(
    antibiotics = html_text(atb, trim = TRUE),
    atb_values = html_attr(atb, name = "value")
  ) |>
    filter(.data$atb_values != "-1")

  bact <- search_page |>
    html_elements(css = "#search_species > option")
  df_bact <- data.frame(
    bacteria = html_text(bact, trim = TRUE),
    bacteria_values = html_attr(bact, name = "value")
  ) |>
    filter(.data$bacteria_values != "-1")

  list(df_atb, df_bact)
}

#' Download a species-specific MIC distribution from EUCAST.
#'
#' @param antibiotic Antibiotic label as shown in the EUCAST search results.
#' @param bact Bacterial species label as shown in the EUCAST search results.
#' @param eucast Lookup tables returned by `update_eucast()`.
#'
#' @return A named list with the raw MIC row, filtered distribution, and ECOFF metadata,
#'   or `NULL` when no matching species is available.
#' @export
mic_distribution <- function(antibiotic, bact, eucast) {
  atb_value <- eucast[[1]] |>
    filter(.data$antibiotics == antibiotic) |>
    select("atb_values") |>
    pull()

  base_url <- c(
    "https://mic.eucast.org/search/?search%5Bmethod%5D=mic&search%5Bantibiotic%5D=",
    "&search%5Bspecies%5D=-1&search%5Bdisk_content%5D=-1&search%5Blimit%5D=99"
  )
  atb_url <- paste0(base_url[1], atb_value, base_url[2])

  mic_page <- read_html(atb_url)
  df_mic <- mic_page |>
    html_element("#search-results-table") |>
    html_table()

  colnames(df_mic)[1] <- "bacteria"
  df_mic <- df_mic |>
    filter(.data$Distributions != "Distributions")

  df_mic_specific <- filter(df_mic, .data$bacteria == bact)

  if (nrow(df_mic_specific) == 0) {
    return(NULL)
  }

  ecoff <- df_mic_specific$`(T)ECOFF`
  ecoff_ci <- df_mic_specific$`Confidence interval`

  mic_col_to_remove <- df_mic_specific[1, ] |>
    select(-c("Distributions", "Observations", "(T)ECOFF", "Confidence interval")) |>
    select(where(~ . == 0)) |>
    colnames()

  mic_distribution <- df_mic_specific |>
    select(-c("bacteria", "Distributions", "Observations", "(T)ECOFF", "Confidence interval")) |>
    select(-all_of(mic_col_to_remove))

  list(
    mic_dataframe = df_mic_specific,
    mic_distribution = mic_distribution,
    ecoff = ecoff,
    ecoff_ci = ecoff_ci
  )
}
