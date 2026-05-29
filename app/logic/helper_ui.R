# set language
#' @export
lang <- "fr"

# data containing the input information used in the UI
#' @export
ui_data <- list(
  fr = list(
    choices = list(
      sex = c(
        "Male" = "Male",
        "Female" = "Female"
      ),
      administration_route = c(
        "Intraveineux" = "IV",
        "Sous Cutanee" = "SC",
        "Intramusculaire" = "IM",
        "Per Os" = "PO",
        "Perfusion Continue" = "CI"
      ),
      target = c(
        "Css > 10 x CMI" = "ten_mic",
        "Css > 4 x CMI" = "four_mic",
        "Css > CMI" = "one_mic",
        "Afficher toutes les cibles" = "all_mic"
      ),
      drug = c(
        "Cefepime" = "Cefepime",
        "Ceftazidime" = "Ceftazidime",
        "Ceftolozane" = "Ceftolozane",
        "Meropenem" = "Meropenem",
        "Cefiderocol" = "Cefiderocol",
        "Piperacilline Tazobactam" = "Piperacillin-tazobactam"
      ),
      administration_interval = c("q48h", "q24h", "q12h", "q8h", "q6h", "q4h", "continue")
    ),
    renal_function_formula = c(
      "Cockcroft and Gault" = "CG",
      "MDRD" = "MDRD",
      "CKD EPI (2009)" = "CKD_2009",
      "CKD EPI (2021)" = "CKD_2021",
      "UV/P" = "UVP",
      "Schwartz" = "schwartz",
      "None" = "none"
    ),
    weight_formula_choices = c(
      "Total body weight" = "None",
      "Ideal Body Weight" = "IBW",
      "Adjusted Body Weight" = "AJBW",
      "Lean Body Weight (Janmahasatian 2005)" = "LBW",
      "BSA (Dubois 1916)" = "BSA"
    ),
    label = list(
      sex = "Sex",
      age = "Age",
      drug = "Antibiotique",
      weight = "Poids total",
      weight_formula = "Formule de poids",
      height = "Taille (cm)",
      creatinine = "Creatinine",
      renal_formula = "Formule fonction renale",
      urinary_creat = "Creatinine urinaire (mmol/L)",
      urinary_output = "Volume urinaire (mL)",
      admin_route = "Voie d administration",
      dose_input = "Dose Antibiotique (g)",
      renal_calc = "Estimer la fonction renale",
      administration_route = "Voie Administration",
      administration_interval = "frequence dAdministration",
      target = "Cible Css/CMI",
      conf_interval = "Interval de Confiance"
    )
  )
)
