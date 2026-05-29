box::use(
  dplyr[filter],
  jsonlite[fromJSON, write_json],
  rvest[html_attr, html_element, html_elements, html_table, html_text, read_html]
)

EUCAST_LOOKUP_PATH <- file.path("app", "static", "eucast.json")
EUCAST_MIC_PATH <- file.path("app", "static", "eucast_mic.json")

empty_eucast_lookup <- function() {
  list(
    data.frame(antibiotics = character(0), atb_values = character(0)),
    data.frame(bacteria = character(0), bacteria_values = character(0))
  )
}

empty_mic_distribution <- function() {
  data.frame(mic = numeric(0), distribution = numeric(0))
}

read_cached_json <- function(path, fallback, context, simplify_vector = TRUE) {
  if (!file.exists(path)) {
    warning(
      paste0("Unable to read cached EUCAST ", context, ": ", path, " does not exist."),
      call. = FALSE
    )
    return(fallback)
  }

  tryCatch(
    fromJSON(path, simplifyVector = simplify_vector),
    error = function(error) {
      warning(
        paste0("Unable to read cached EUCAST ", context, ": ", conditionMessage(error)),
        call. = FALSE
      )
      fallback
    }
  )
}

write_cached_json <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_json(data, path, auto_unbox = TRUE, null = "null", pretty = TRUE)
}

read_eucast_page <- function(url, context) {
  tryCatch(
    read_html(url),
    error = function(error) {
      warning(
        paste0("Unable to download EUCAST ", context, ": ", conditionMessage(error)),
        call. = FALSE
      )
      NULL
    }
  )
}

extract_eucast_lookup <- function(search_page) {
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

build_mic_url <- function(atb_value) {
  paste0(
    "https://mic.eucast.org/search/?search%5Bmethod%5D=mic&search%5Bantibiotic%5D=",
    atb_value,
    "&search%5Bspecies%5D=-1&search%5Bdisk_content%5D=-1&search%5Blimit%5D=99"
  )
}

extract_mic_table <- function(mic_page) {
  df_mic <- tryCatch(
    mic_page |>
      html_element("#search-results-table") |>
      html_table(),
    error = function(error) {
      warning(
        paste0("Unable to parse EUCAST MIC distribution table: ", conditionMessage(error)),
        call. = FALSE
      )
      NULL
    }
  )

  if (is.null(df_mic)) {
    return(NULL)
  }

  colnames(df_mic)[1] <- "bacteria"
  df_mic |>
    filter(.data$Distributions != "Distributions")
}

format_mic_entry <- function(mic_row) {
  distribution_columns <- setdiff(
    colnames(mic_row),
    c("bacteria", "Distributions", "Observations", "(T)ECOFF", "Confidence interval")
  )

  mic_distribution <- data.frame(
    mic = suppressWarnings(as.numeric(distribution_columns)),
    distribution = as.numeric(unlist(mic_row[1, distribution_columns, drop = FALSE], use.names = FALSE))
  )

  mic_distribution <- mic_distribution[
    is.finite(mic_distribution$mic) &
      is.finite(mic_distribution$distribution) &
      mic_distribution$distribution > 0,
    ,
    drop = FALSE
  ]

  rownames(mic_distribution) <- NULL

  list(
    mic_distribution = mic_distribution,
    ecoff = as.character(mic_row$`(T)ECOFF`[[1]]),
    ecoff_ci = as.character(mic_row$`Confidence interval`[[1]])
  )
}

normalize_cached_mic_distribution <- function(mic_distribution) {
  if (is.null(mic_distribution)) {
    return(empty_mic_distribution())
  }

  normalized_distribution <- if (is.data.frame(mic_distribution)) {
    mic_distribution
  } else if (!length(mic_distribution)) {
    empty_mic_distribution()
  } else {
    data.frame(
      mic = as.numeric(vapply(mic_distribution, function(row) row[["mic"]], numeric(1))),
      distribution = as.numeric(vapply(mic_distribution, function(row) row[["distribution"]], numeric(1)))
    )
  }

  normalized_distribution <- normalized_distribution[
    is.finite(normalized_distribution$mic) &
      is.finite(normalized_distribution$distribution) &
      normalized_distribution$distribution > 0,
    ,
    drop = FALSE
  ]

  rownames(normalized_distribution) <- NULL
  normalized_distribution
}

#' Read the stored EUCAST antibiotic and species lists.
#'
#' @param path Path to the JSON file that stores the antibiotic and species lookups.
#'
#' @return A list with the antibiotic lookup table and the species lookup table.
#' @export
update_eucast <- function(path = EUCAST_LOOKUP_PATH) {
  eucast <- read_cached_json(path, empty_eucast_lookup(), "lookup tables")

  if (!is.list(eucast) || length(eucast) < 2) {
    return(empty_eucast_lookup())
  }

  df_atb <- as.data.frame(eucast[[1]], stringsAsFactors = FALSE)
  df_bact <- as.data.frame(eucast[[2]], stringsAsFactors = FALSE)

  if (!all(c("antibiotics", "atb_values") %in% colnames(df_atb))) {
    return(empty_eucast_lookup())
  }

  if (!all(c("bacteria", "bacteria_values") %in% colnames(df_bact))) {
    return(empty_eucast_lookup())
  }

  list(
    df_atb[, c("antibiotics", "atb_values"), drop = FALSE],
    df_bact[, c("bacteria", "bacteria_values"), drop = FALSE]
  )
}

#' Read the stored EUCAST MIC cache.
#'
#' @param path Path to the JSON file that stores MIC distributions.
#'
#' @return A nested list keyed by antibiotic and then by bacterium.
#' @export
read_eucast_mic <- function(path = EUCAST_MIC_PATH) {
  read_cached_json(path, list(), "MIC distributions", simplify_vector = FALSE)
}

#' Download and store the current EUCAST antibiotic and species lists.
#'
#' @param output_path Path to the JSON file that stores the antibiotic and species lookups.
#'
#' @return A list with the antibiotic lookup table and the species lookup table.
#' @export
download_eucast_lookup <- function(output_path = EUCAST_LOOKUP_PATH) {
  search_page <- read_eucast_page("https://mic.eucast.org/search/", "lookup tables")

  if (is.null(search_page)) {
    return(empty_eucast_lookup())
  }

  eucast <- extract_eucast_lookup(search_page)
  write_cached_json(eucast, output_path)
  eucast
}

#' Download and store the full EUCAST MIC cache.
#'
#' @param eucast Lookup tables returned by `update_eucast()` or `download_eucast_lookup()`.
#' @param output_path Path to the JSON file that stores all cached MIC distributions.
#'
#' @return A nested list keyed by antibiotic and then by bacterium.
#' @export
download_eucast_mic <- function(eucast = update_eucast(), output_path = EUCAST_MIC_PATH) {
  if (
    length(eucast) < 1 ||
    !is.data.frame(eucast[[1]]) ||
    !nrow(eucast[[1]])
  ) {
    return(list())
  }

  mic_cache <- list()

  for (index in seq_len(nrow(eucast[[1]]))) {
    antibiotic <- eucast[[1]]$antibiotics[[index]]
    atb_value <- eucast[[1]]$atb_values[[index]]
    mic_page <- read_eucast_page(build_mic_url(atb_value), paste0("MIC table for ", antibiotic))

    if (is.null(mic_page)) {
      next
    }

    mic_table <- extract_mic_table(mic_page)

    if (is.null(mic_table) || !nrow(mic_table)) {
      next
    }

    antibiotic_mic <- list()

    for (row_index in seq_len(nrow(mic_table))) {
      bacteria <- mic_table$bacteria[[row_index]]
      mic_entry <- format_mic_entry(mic_table[row_index, , drop = FALSE])

      if (!nrow(mic_entry$mic_distribution)) {
        next
      }

      antibiotic_mic[[bacteria]] <- mic_entry
    }

    if (length(antibiotic_mic)) {
      mic_cache[[antibiotic]] <- antibiotic_mic
    }
  }

  write_cached_json(mic_cache, output_path)
  mic_cache
}

#' Read a species-specific MIC distribution from the stored EUCAST MIC cache.
#'
#' @param antibiotic Antibiotic label as shown in the EUCAST lookup file.
#' @param bact Bacterial species label as shown in the EUCAST lookup file.
#' @param eucast_mic Nested MIC cache returned by `read_eucast_mic()`.
#'
#' @return A named list with the filtered distribution and ECOFF metadata,
#'   or `NULL` when no matching species is available.
#' @export
mic_distribution <- function(antibiotic, bact, eucast_mic) {
  if (!length(eucast_mic)) {
    return(NULL)
  }

  antibiotic_cache <- eucast_mic[[antibiotic]]

  if (is.null(antibiotic_cache)) {
    return(NULL)
  }

  mic_entry <- antibiotic_cache[[bact]]

  if (is.null(mic_entry)) {
    return(NULL)
  }

  mic_entry$mic_distribution <- normalize_cached_mic_distribution(mic_entry$mic_distribution)

  if (!nrow(mic_entry$mic_distribution)) {
    return(NULL)
  }

  mic_entry
}
