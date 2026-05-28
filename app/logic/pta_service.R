box::use(
  dplyr,
  ggplot2,
  rvest,
  scales,
  stats
)

box::use(
  app/logic/utils[get_cv_from_sd, get_sd_from_cv]
)

bsa <- function(height, weight, capped = FALSE, formula = "dubois") {
  if (!is.logical(capped)) {
    stop("Error: capped should be TRUE or FALSE")
  }

  if (isTRUE(formula == "dubois")) {
    body_surface_area <- 0.007184 * height^0.725 * weight^0.425
  }

  if (isTRUE(capped) && body_surface_area > 2) {
    body_surface_area <- 2
  }

  round(body_surface_area, digits = 2)
}

weight_formula <- function(
  weight,
  height,
  sex,
  weight_unit = c("kg", "lbs"),
  formula = c("IBW", "AJBW", "LBW")
) {
  weight <- ifelse(weight_unit == "lbs", weight / 2.20462, weight)
  bmi <- weight / (height / 100)^2
  height_inch <- (height - 152.4) / 2.54
  ibw <- ifelse(sex == "Female", 45.5, 50) + 2.3 * ifelse(height_inch > 0, height_inch, 0)
  ajbw <- ibw + 0.4 * (weight - ibw)
  ffm <- 9270 * weight / (ifelse(sex == "Female", 8780 + 244 * bmi, 6680 + 216 * bmi))

  mod_weight <- dplyr$case_when(
    formula == "IBW" ~ ibw,
    formula == "AJBW" ~ ajbw,
    formula == "LBW" ~ ffm
  )

  round(mod_weight, digits = 1)
}

renal_function <- function(
  sex,
  age,
  weight,
  height,
  creat,
  ethnicity = "Caucasian",
  formula,
  creat_unit = "uM/L",
  urine_creat = 0,
  urine_output = 0
) {
  if (isTRUE(creat_unit == "uM/L")) {
    creat_mgdl <- creat / 88.4
    creat_micromol <- creat
    urine_creat_mmol <- urine_creat
  }

  if (isTRUE(creat_unit == "mg/dL")) {
    creat_mgdl <- creat
    creat_micromol <- creat * 88.4
    urine_creat_mmol <- urine_creat * 8.84
  }

  k <- ifelse(sex == "Male", 0.9, 0.7)
  beta_cg <- ifelse(sex == "Male", 1.23, 1.04)
  alpha_2009 <- ifelse(sex == "Male", -0.411, -0.329)
  alpha_2021 <- ifelse(sex == "Male", -0.302, -0.241)
  ckd_female_corr <- ifelse(sex == "Male", 1, 1.018)
  ckd_ethnic_correction <- ifelse(ethnicity == "African", 1.159, 1)
  mdrd_female_corr <- ifelse(sex == "Male", 1, 0.742)
  mdrd_ethnic_correction <- ifelse(ethnicity == "African", 1.212, 1)

  if (formula == "CG") {
    out <- beta_cg * (140 - age) * (weight / creat_micromol)
  }

  if (formula == "MDRD") {
    out <- 175 * creat_mgdl^-1.154 * ifelse(age == 0, 1, age)^-0.203
    out <- out * mdrd_female_corr * mdrd_ethnic_correction
  }

  if (formula == "CKD_2009") {
    out <- 141 * min(creat_mgdl / k, 1)^alpha_2009 * max(creat_mgdl / k, 1)^-1.209
    out <- out * 0.993^age * ckd_female_corr * ckd_ethnic_correction
  }

  if (formula == "CKD_2021") {
    out <- 142 * min(creat_mgdl / k, 1)^alpha_2021 * max(creat_mgdl / k, 1)^-1.20
    out <- out * 0.993^age * ckd_female_corr
  }

  if (formula == "UVP") {
    out <- urine_creat_mmol * urine_output / creat_micromol / (24 * 60) * 1000
  }

  if (formula == "schwartz") {
    k <- ifelse(age < 12, 0.55, 0.7)
    out <- k * height / creat_mgdl
  }

  if (formula == "EKFC") {
    exponent <- ifelse((creat_mgdl / k) < 1, -0.322, -1.132)
    age_exponent <- ifelse(age > 40, age - 40, 0)
    out <- 107.3 * (creat_mgdl / k)^exponent * 0.990^age_exponent
  }

  if (formula == "none") {
    out <- 1
  }

  round(out, digits = 1)
}

#' @export
calc_biological <- function(
  sex,
  age,
  weight,
  height,
  creatinine,
  weight_unit,
  creat_unit,
  urine_creat,
  urine_output
) {
  tbw <- weight
  lbw <- weight_formula(weight, height, sex, weight_unit, formula = "LBW")
  ajbw <- weight_formula(weight, height, sex, weight_unit, formula = "AJBW")
  ibw <- weight_formula(weight, height, sex, weight_unit, formula = "IBW")
  bmi <- round(weight / (height / 100)^2, digits = 1)
  bsa_value <- bsa(height, weight)
  cg_tbw <- renal_function(sex, age, tbw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_ajbw <- renal_function(sex, age, ajbw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_ibw <- renal_function(sex, age, ibw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  cg_lbw <- renal_function(sex, age, lbw, height, creatinine, formula = "CG", creat_unit = creat_unit)
  mdrd <- renal_function(sex, age, tbw, height, creatinine, formula = "MDRD", creat_unit = creat_unit)
  ckd_2009 <- renal_function(sex, age, tbw, height, creatinine, formula = "CKD_2009", creat_unit = creat_unit)
  ckd_2021 <- renal_function(sex, age, tbw, height, creatinine, formula = "CKD_2021", creat_unit = creat_unit)
  schwartz <- renal_function(sex, age, tbw, height, creatinine, formula = "schwartz", creat_unit = creat_unit)
  uvp <- renal_function(
    sex,
    age,
    tbw,
    height,
    creatinine,
    formula = "UVP",
    creat_unit = creat_unit,
    urine_creat = urine_creat,
    urine_output = urine_output
  )
  ekfc <- renal_function(sex, age, tbw, height, creatinine, formula = "EKFC", creat_unit = creat_unit)

  list(
    tbw = tbw,
    lbw = lbw,
    ajbw = ajbw,
    ibw = ibw,
    bmi = bmi,
    bsa = bsa_value,
    cg_tbw = cg_tbw,
    cg_ajbw = cg_ajbw,
    cg_ibw = cg_ibw,
    cg_lbw = cg_lbw,
    mdrd = mdrd,
    ckd_2009 = ckd_2009,
    ckd_2021 = ckd_2021,
    schwartz = schwartz,
    uvp = uvp,
    ekfc = ekfc
  )
}

#' @export
get_model_parameters <- function(model, biological, drug = NULL) {
  cl <- switch(
    model,
    "Barreto_2023" = 7.84,
    "Cacqueray_2022" = 1.21 * (biological$tbw / 9)^0.75 * (biological$schwartz / 153)^0.37,
    "An_2023" = 0.526 + 2 * biological$cg_lbw / 54,
    "Buning_2021" = 3.42 * (biological$ckd_2009 / 73)^0.772,
    "Launay_2024" = 4.45 * (biological$ckd_2009 / 73.9)^0.9,
    "Cojutti_2024" = 5 * (biological$ekfc / 70)^0.7,
    "Zhar_2022" = 7.38 * (biological$ckd_2009 / 100)^0.467,
    "Chandorkar_2015" = 5.11 * 1.215 * (biological$cg_tbw / 109)^0.715,
    "Zhang_2021" = 4.84 * (biological$cg_tbw / 100)^0.701,
    "Gijsen_2021" = 1,
    "Minichmayr_2018" = 1,
    "Ehrmann_2019" = 1,
    "Huang_2025" = 1,
    "Lan_2022" = 1,
    "Fukumoto_2023" = 1.35 * ((biological$uvp * 1.73 / biological$bsa) / 87.6)^0.67,
    "Klastrup_2020" = 2.25 + 0.119 * biological$cg_tbw,
    "Sukarnjanaset_2019" = 5.37 + 0.06 * (biological$cg_tbw - 55),
    "Udy_2015" = 16.3 * (biological$cg_tbw / 100),
    1
  )

  eta_cl <- switch(
    model,
    "Barreto_2023" = 1,
    "Cacqueray_2022" = 0.39,
    "An_2023" = get_sd_from_cv(0.299),
    "Buning_2021" = get_sd_from_cv(0.36),
    "Launay_2024" = 0.46,
    "Cojutti_2024" = get_sd_from_cv(0.6792),
    "Zhar_2022" = 0.467,
    "Chandorkar_2015" = get_sd_from_cv(0.33),
    "Zhang_2021" = get_sd_from_cv(0.429),
    "Gijsen_2021" = 1,
    "Minichmayr_2018" = 1,
    "Ehrmann_2019" = 1,
    "Huang_2025" = 1,
    "Lan_2022" = 1,
    "Fukumoto_2023" = get_sd_from_cv(0.221),
    "Klastrup_2020" = 0.533,
    "Sukarnjanaset_2019" = get_sd_from_cv(0.285),
    "Udy_2015" = get_cv_from_sd(0.56),
    1
  )

  dose_increment <- dplyr$case_when(
    drug == "Amoxicillin" ~ 0.500,
    drug == "Cefepime" ~ 1.000,
    drug == "Cefazoline" ~ 0.500,
    drug == "Cefotaxim" ~ 0.500,
    drug == "Cefiderocol" ~ 1.000,
    drug == "Ceftazidime" ~ 1.000,
    drug == "Ceftaroline" ~ 1.000,
    drug == "Ceftobiprol" ~ 1.000,
    drug == "Ceftolozane" ~ 1.000,
    drug == "Piperacillin-tazobactam" ~ 2.000,
    drug == "Meropenem" ~ 0.500,
    TRUE ~ 0
  )

  list(cl = cl, eta_cl = eta_cl, dose_increment = dose_increment)
}

#' @export
drug_threshold <- function(drug) {
  switch(
    drug,
    "Amoxicillin" = NA,
    "Cefepime" = 20,
    "Cefazoline" = NA,
    "Cefotaxim" = NA,
    "Cefiderocol" = NA,
    "Ceftazidime" = NA,
    "Ceftaroline" = NA,
    "Ceftobiprol" = NA,
    "Ceftolozane" = NA,
    "Piperacillin-tazobactam" = 157,
    "Meropenem" = 45,
    0
  )
}

#' @export
max_dose <- function(drug) {
  switch(
    drug,
    "Amoxicillin" = 20,
    "Cefepime" = 20,
    "Cefazoline" = 20,
    "Cefotaxim" = 20,
    "Cefiderocol" = 20,
    "Ceftazidime" = 20,
    "Ceftaroline" = 20,
    "Ceftobiprol" = 20,
    "Ceftolozane" = 20,
    "Piperacillin-tazobactam" = 40,
    "Meropenem" = 20,
    0
  )
}

#' @export
get_default_model <- function(drug) {
  switch(
    drug,
    "Cefepime" = "An_2023",
    "Ceftazidime" = "Buning_2021",
    "Ceftolozane" = "Zhang_2021",
    "Cefiderocol" = "Zhar_2022",
    "Piperacillin-tazobactam" = "Klastrup_2020",
    "Meropenem" = "Ehrmann_2019",
    character(0)
  )
}

#' @export
update_eucast <- function() {
  search_page <- rvest$read_html("https://mic.eucast.org/search/")

  atb <- search_page |>
    rvest$html_elements(css = "#search_antibiotic > option")
  df_atb <- data.frame(
    antibiotics = rvest$html_text(atb, trim = TRUE),
    atb_values = rvest$html_attr(atb, name = "value")
  ) |>
    dplyr$filter(.data$atb_values != "-1")

  bact <- search_page |>
    rvest$html_elements(css = "#search_species > option")
  df_bact <- data.frame(
    bacteria = rvest$html_text(bact, trim = TRUE),
    bacteria_values = rvest$html_attr(bact, name = "value")
  ) |>
    dplyr$filter(.data$bacteria_values != "-1")

  list(df_atb, df_bact)
}

#' @export
mic_distribution <- function(antibiotic, bact, eucast) {
  atb_value <- eucast[[1]] |>
    dplyr$filter(.data$antibiotics == antibiotic) |>
    dplyr$select("atb_values") |>
    dplyr$pull()

  base_url <- c(
    "https://mic.eucast.org/search/?search%5Bmethod%5D=mic&search%5Bantibiotic%5D=",
    "&search%5Bspecies%5D=-1&search%5Bdisk_content%5D=-1&search%5Blimit%5D=99"
  )
  atb_url <- paste0(base_url[1], atb_value, base_url[2])

  mic_page <- rvest$read_html(atb_url)
  df_mic <- mic_page |>
    rvest$html_element("#search-results-table") |>
    rvest$html_table()

  colnames(df_mic)[1] <- "bacteria"
  df_mic <- df_mic |>
    dplyr$filter(.data$Distributions != "Distributions")

  df_mic_specific <- dplyr$filter(df_mic, .data$bacteria == bact)

  if (nrow(df_mic_specific) == 0) {
    return(NULL)
  }

  ecoff <- df_mic_specific$`(T)ECOFF`
  ecoff_ci <- df_mic_specific$`Confidence interval`

  mic_col_to_remove <- df_mic_specific[1, ] |>
    dplyr$select(-c("Distributions", "Observations", "(T)ECOFF", "Confidence interval")) |>
    dplyr$select(dplyr$where(~ . == 0)) |>
    colnames()

  distribution <- df_mic_specific |>
    dplyr$select(-c("bacteria", "Distributions", "Observations", "(T)ECOFF", "Confidence interval")) |>
    dplyr$select(-dplyr$all_of(mic_col_to_remove))

  list(
    mic_dataframe = df_mic_specific,
    mic_distribution = distribution,
    ecoff = ecoff,
    ecoff_ci = ecoff_ci
  )
}

calc_css_distribution <- function(dose, tvcl, eta_cl, n_sim = 50000) {
  set.seed(3917985)

  if (n_sim == 0) {
    cl_distribution <- tvcl
  }

  if (n_sim > 0) {
    cl_distribution <- tvcl * stats$rlnorm(n_sim, meanlog = 0, sdlog = eta_cl)
  }

  (dose / 24) / cl_distribution
}

positive_finite_values <- function(...) {
  values <- unlist(list(...), use.names = FALSE)
  values[is.finite(values) & !is.na(values) & values > 0]
}

safe_log_limits <- function(..., lower_floor = 0.01, fallback_upper = 1) {
  values <- positive_finite_values(...)

  if (!length(values)) {
    return(c(lower_floor, fallback_upper))
  }

  lower <- max(lower_floor, min(values))
  upper <- max(values)

  if (!is.finite(upper) || upper <= lower) {
    upper <- max(lower * 2, fallback_upper)
  }

  c(lower, upper)
}

threshold_curve <- function(threshold, mic) {
  if (length(threshold) == 1 && (is.na(threshold) || threshold <= 0)) {
    return(rep(NA_real_, length(mic)))
  }

  threshold / mic
}

#' @export
sim_concentration <- function(
  dose,
  tvcl,
  eta_cl,
  quantile = c(0.025, 0.975),
  mic = NA,
  dose_increment = 0,
  toxicity_threshold,
  additional_threshold = NA_real_,
  n_sim = 50000
) {
  if (length(mic) == 1 && is.na(mic)) {
    mic <- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64)
  }

  css_distribution <- calc_css_distribution(dose, tvcl, eta_cl, n_sim)
  quant <- stats$quantile(css_distribution, probs = quantile)
  dose_range <- c(-2, -1, 0, 1, 2) * dose_increment + dose
  tv_css_range <- dose_range / (tvcl * 24)

  css_mic_range <- data.frame(
    css_mic_below2 = tv_css_range[1] / mic,
    css_mic_below1 = tv_css_range[2] / mic,
    css_mic_above1 = tv_css_range[4] / mic,
    css_mic_above2 = tv_css_range[5] / mic
  )

  quantile_df <- data.frame(
    css_mic = tv_css_range[3] / mic,
    mic = mic,
    percentile_2.5 = quant[1] / mic,
    percentile_97.5 = quant[2] / mic
  )

  concentration_df <- dplyr$bind_cols(
    quantile_df,
    css_mic_range,
    toxicity_threshold = threshold_curve(toxicity_threshold, mic),
    additional_threshold = threshold_curve(additional_threshold, mic)
  )

  round(concentration_df, digits = 4)
}

calculate_cfr <- function(
  tvcl,
  eta_cl,
  dose,
  mic_distribution,
  toxicity_threshold = NULL,
  n_sim = 50000
) {
  if (!is.data.frame(mic_distribution)) {
    stop("mic_distribution must be a dataframe")
  }

  cfr <- 0
  mic_distribution <- dplyr$mutate(
    mic_distribution,
    relative_distribution = distribution / sum(distribution)
  )
  css_distribution <- calc_css_distribution(dose, tvcl, eta_cl, n_sim = n_sim)

  for (index in seq_len(nrow(mic_distribution))) {
    css_distribution_mic <- mean(css_distribution > mic_distribution$mic[index])
    css_distribution_mic <- css_distribution_mic * mic_distribution$relative_distribution[index]
    cfr <- sum(cfr, css_distribution_mic)
  }

  toxicity_proportion <- NULL
  if (!is.null(toxicity_threshold) && is.finite(toxicity_threshold) && toxicity_threshold > 0) {
    toxicity_proportion <- mean(css_distribution > toxicity_threshold)
  }

  list(cfr = cfr, toxicity_proportion = toxicity_proportion)
}

#' @export
calculate_cfr_mulitple_doses <- function(
  dose_increment,
  dose_max,
  tvcl,
  eta_cl,
  mic_distribution,
  toxicity_threshold = NULL,
  n_sim = 50000
) {
  dosing_sequence <- seq(0, dose_max, dose_increment)

  for (index in seq_along(dosing_sequence)) {
    cfr <- calculate_cfr(
      tvcl,
      eta_cl,
      dosing_sequence[index],
      mic_distribution,
      toxicity_threshold,
      n_sim
    )

    current_row <- data.frame(
      dose = dosing_sequence[index],
      cfr = cfr$cfr,
      toxicity_proportion = cfr$toxicity_proportion
    )

    if (index == 1) {
      cfr_df <- current_row
    } else {
      cfr_df <- rbind(cfr_df, current_row)
    }
  }

  cfr_df
}

#' @export
plot.pta <- function(data, ecoff = NA) {
  x_limits <- safe_log_limits(data$mic)
  y_limits <- safe_log_limits(
    data$css_mic_below2,
    data$css_mic_below1,
    data$css_mic,
    data$css_mic_above1,
    data$css_mic_above2,
    data$percentile_2.5,
    data$percentile_97.5,
    data$toxicity_threshold,
    data$additional_threshold
  )

  pta_plot <- ggplot2$ggplot(data = data) +
    ggplot2$geom_hline(mapping = ggplot2$aes(yintercept = 1), col = "#2b94ab", lty = 2, lwd = 0.5) +
    ggplot2$geom_hline(mapping = ggplot2$aes(yintercept = 4), col = "#0e877b", lty = 2, lwd = 0.5) +
    ggplot2$geom_line(mapping = ggplot2$aes(x = .data$mic, y = .data$css_mic), col = "#2db391", lty = 1, lwd = 1) +
    ggplot2$labs(linetype = NULL) +
    ggplot2$scale_x_continuous(
      trans = "log2",
      breaks = data$mic,
      labels = data$mic,
      limits = x_limits
    ) +
    ggplot2$scale_y_continuous(
      trans = "log2",
      n.breaks = 10,
      limits = y_limits
    ) +
    ggplot2$xlab("MIC (mg/L)") +
    ggplot2$ylab("Css/MIC") +
    ggplot2$theme_bw(base_size = 14) +
    ggplot2$theme(
      legend.position = "inside",
      legend.justification.inside = c(0.9, 0.9),
      legend.box.background = ggplot2$element_rect()
    )

  if (any(is.finite(data$toxicity_threshold) & !is.na(data$toxicity_threshold))) {
    pta_plot <- pta_plot + ggplot2$geom_line(
      mapping = ggplot2$aes(x = .data$mic, y = .data$toxicity_threshold),
      col = "#960b0b",
      lty = 1,
      lwd = 0.9
    )
  }

  if (any(is.finite(data$additional_threshold) & !is.na(data$additional_threshold))) {
    pta_plot <- pta_plot + ggplot2$geom_line(
      mapping = ggplot2$aes(x = .data$mic, y = .data$additional_threshold),
      col = "#f2a65a",
      lty = 3,
      lwd = 0.9
    )
  }

  if (!is.na(ecoff)) {
    pta_plot <- pta_plot + ggplot2$geom_vline(
      ggplot2$aes(xintercept = ecoff, linetype = "ECOFF"),
      lwd = 1,
      col = "black"
    )
  }

  pta_multiple_doses <- pta_plot +
    ggplot2$geom_line(
      data = data,
      mapping = ggplot2$aes(x = .data$mic, y = .data$css_mic_below1),
      col = "#20846b",
      lty = 1,
      lwd = 1
    ) +
    ggplot2$geom_line(
      data = data,
      mapping = ggplot2$aes(x = .data$mic, y = .data$css_mic_below2),
      col = "#1f8269",
      lty = 1,
      lwd = 1
    ) +
    ggplot2$geom_line(
      data = data,
      mapping = ggplot2$aes(x = .data$mic, y = .data$css_mic_above1),
      col = "#32c5a0",
      lty = 1,
      lwd = 1
    ) +
    ggplot2$geom_line(
      data = data,
      mapping = ggplot2$aes(x = .data$mic, y = .data$css_mic_above2),
      col = "#2fe3b6",
      lty = 1,
      lwd = 1
    )

  pta_ci_plot <- pta_plot +
    ggplot2$geom_ribbon(
      data = data,
      ggplot2$aes(
        ymin = .data$percentile_2.5,
        ymax = .data$percentile_97.5,
        x = .data$mic
      ),
      fill = "#0889f1",
      alpha = 0.1,
      col = "#0889f1"
    )

  list(
    pta_plot = pta_plot,
    pta_multiple_doses = pta_multiple_doses,
    pta_ci_plot = pta_ci_plot
  )
}

#' @export
plot.cfr <- function(data, dose_increment = 0) {
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }

  required_columns <- c("dose", "cfr", "toxicity_proportion")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "The data frame is missing the following columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  cfr_plot <- ggplot2$ggplot(data, ggplot2$aes(x = .data$dose / 1000)) +
    ggplot2$geom_line(ggplot2$aes(y = .data$cfr), col = "#2db391", lty = 1, lwd = 1)

  if (any(is.finite(data$toxicity_proportion) & !is.na(data$toxicity_proportion))) {
    cfr_plot <- cfr_plot + ggplot2$geom_line(
      ggplot2$aes(y = .data$toxicity_proportion),
      col = "#960b0b"
    )
  }

  cfr_plot +
    ggplot2$scale_x_continuous(trans = scales$pseudo_log_trans()) +
    ggplot2$xlab("Dose (g)") +
    ggplot2$ylab("CFR (%)") +
    ggplot2$geom_hline(yintercept = 0.1, col = "#2b94ab", lty = 2, lwd = 0.5) +
    ggplot2$geom_hline(yintercept = 0.9, col = "#0e877b", lty = 2, lwd = 0.5) +
    ggplot2$theme_bw(base_size = 14) +
    ggplot2$theme(
      legend.position = "inside",
      legend.justification.inside = c(0.9, 0.9),
      legend.box.background = ggplot2$element_rect()
    )
}