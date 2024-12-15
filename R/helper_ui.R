# set language
lang <- "fr"

# data containing the input information used in the UI
ui_data <- list(
  fr = list(
    choices = list(
      sex = c(
        "Homme" = "Male",
        "Femme" = "Female"
      ),
      administration_route = c(
        "Intraveineux" = "IV",
        "Sous Cutanee" = "SC",
        "Intramusculaire" = "IM",
        "Per Os" = "PO",
        "Perfusion Continue" = "CI"
      ),
      drug = c(
        "Amoxicilline" = "amoxicillin",
        "Cefepime" = "cefepim",
        "Cefotaxime" = "cefotaxim",
        "Meropenem" = "meropenem",
        "Piperacilline Tazobactam" = "piptazo"
      )
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
      sex = "Sexe",
      age = "Age",
      drug = "Antibiotique",
      weight = "Poids total (kg)",
      weight_formula = "Formule de poids",
      height = "Taille (cm)",
      creatinine = "Creatinine (umol/L)",
      renal_formula = "Formule fonction renale",
      urinary_creat = "Creatinine urinaire (mmol/L)",
      urinary_output = "Volume urinaire (mL)",
      admin_route = "Voie d administration",
      dose_input = "Dose Antibiotique (g)",
      renal_calc = "Estimer la fonction renale",
      administration_route = "Voie Administration"
    )
  )
)
