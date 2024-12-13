
# by definition eta and gamma value are always numerical.
# if the CV value is found in the original publication, eta and gamma must be recalculated
model_bank <- list(
  amoxicillin = list(NA),
  cefepim = list(
    cacqueray_2022 = list(
      tvcl = "1.21 * (tbw / 9) ^ 0.75 * (schwartz / 153) ^ 0.37",
      tvv = "4.8 * tbw / 9",
      eta_cl = 0.39,
      eta_v = 0.35
    ),
    guohua_2023 = list(
      tvcl = "0.526 + 2 * cg_lbw/54",
      eta_cl = 0.293
    )
  ),
  cefotaxim = list(NA),
  meropenem = list(NA),
  piptazo = list(
    klastrup_2020 = list(
      tvcl = "2.25 + 0.119 * cg_tbw",
      eta_cl = 0.533
    )
  )
)

# object to get clearance model_bank[[drug]][[model]][[parameters]]
# test model https://www.sciencedirect.com/science/article/pii/S1198743X22002634s
