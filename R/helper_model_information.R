# Create a tibble to store information about the pharmacokinetics model

model_information <- list(
  "pipetazo" = list(
    klastrup_2020 = tibble::tibble(
      Title = "Pharmacokinetic Model of Piperacillin-Tazobactam",
      Authors = "Klastrup, I. D., et al.",
      Year = 2020,
      Journal = "Journal of Antimicrobial Chemotherapy",
      DOI = "10.1128/AAC.02556-19",
      URL = "https://journals.asm.org/doi/10.1128/aac.02556-19",
      Abstract = "
      <br> <b> Introduction : </b> Pharmacokinetic changes are often seen in patients with severe infections. Administration by continuous infusion has been suggested to optimize
      antibiotic exposure and pharmacokinetic/pharmacodynamic (PK/PD) target attainment for β-lactams. 
      <br> <strong> Methods : </strong> In an observational study, unbound piperacillin concentrations (n = 196) were assessed in 78
      critically ill patients following continuous infusion of piperacillin-tazobactam (ratio 8:1). The initial dose of 8, 12, or 16 g (piperacillin component) was determined
      by individual creatinine clearance (CRCL). Piperacillin concentrations were compared to the EUCAST clinical breakpoint MIC for Pseudomonas aeruginosa (16 mg/liter),
      and the following PK/PD targets were evaluated: 100% free time (fT) > 1× MIC and 100% fT > 4× MIC. A population pharmacokinetic model was developed using NONMEM 7.4.3
      consisting of a one-compartment disposition model with linear elimination separated into nonrenal and renal (linearly increasing with patient CRCL) clearances.
      Target attainment was predicted and visualized for all individuals based on the utilized CRCL dosing algorithm.
      <br> <strong> Results : </strong> The target of 100% fT > 1× MIC was achieved for all patients based on the administered dose, but few patients achieved the target of
      100% fT > 4× MIC. Probability of target attainment for a simulated cohort of patients showed that increasing the daily dose by 4-g increments (piperacillin component) 
      did not result in substantially improved target attainment for the 100% fT > 4× MIC target. 
      <br> <strong> Conclusion : </strong> To conclude, in patients with high CRCL combined with high-MIC bacterial infections, even a continuous infusion (CI) 
      regimen with a daily dose of 24 g may be insufficient to achieve therapeutic concentrations.",
      Key_Findings = "The model accurately predicts the pharmacokinetics of piperacillin and tazobactam in critically ill patients, and can be used to optimize dosing regimens.",
      Clearance_Formula = "$$ CL = 2.25 + 0.119  CG_{TBW} $$",
      Model_Description = "non renal clearance = 2.25 L/h, renal clearance = 0.119 x CRCL calculated with Cockcroft and Gault using the Total Body Weight (TBW).",
      Population_Studied = "Critically ill patients"
    ),
    # Added references for pipetazo studies:
    Sukarnjanaset_2019 = tibble::tibble(
      Title = "Title placeholder for Sukarnjanaset et al., JPP 2019",
      Authors = "Sukarnjanaset, et al.",
      Year = 2019,
      Journal = "Journal of Pediatric Pharmacology",
      DOI = NA,
      URL = "https://pubmed.ncbi.nlm.nih.gov/30963365/",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    ),
    Udy_2015 = tibble::tibble(
      Title = "Augmented Renal Clearance: Implications for Antibiotic Dosing in Critically Ill Patients",
      Authors = "Udy, et al.",
      Year = 2015,
      Journal = "Critical Care",
      DOI = "10.1186/s13054-015-0750-y",
      URL = "https://doi.org/10.1186/s13054-015-0750-y",
      Abstract = "This study investigates augmented renal clearance in critically ill patients and its impact on antibiotic pharmacokinetics. The findings emphasize the need for dose adjustments to achieve optimal therapeutic outcomes in patients exhibiting enhanced renal elimination.",
      Clearance_Formula = "N/A",
      Model_Description = "The study characterizes augmented renal clearance and discusses its implications on the dosing regimens of renally excreted drugs in critically ill patients.",
      Population_Studied = "Critically ill patients"
    )
  ),
  "cefepim" = list(
    cacqueray_2022 = tibble::tibble(
      Title = "Cefepime population pharmacokinetics and dosing regimen optimization in critically ill children with different renal functions",
      Authors = "Cacqueray, A., et al.",
      Year = 2022,
      Journal = "Clinical Microbiology and Infection",
      DOI = "10.1016/j.cmi.2022.05.007",
      URL = "https://pubmed.ncbi.nlm.nih.gov/35605841/",
      Abstract = "Objective: Cefepime is commonly used in pediatric intensive care units, where unpredictable variations in the patients' pharmacokinetic (PK) variables may require drug dose adjustments. <br> Methods: Children (aged from 1 month to 18 years; body weight >3 kg) receiving cefepime were included... ",
      Clearance_Formula = "$$\nCL = 1.21 * \\left(\\frac{tbw}{9}\\right)^{0.75} * \\left(\\frac{schwartz}{153}\\right)^{0.37}\n$$",
      Model_Description = "The model includes parameters such as clearance (CL), volume of distribution (V) ...",
      Population_Studied = "Children (aged from 1 month to 18 years; body weight >3 kg) receiving cefepime"
    ),
    # Added references for cefepim studies:
    An_2023 = tibble::tibble(
      Title = "Population Pharmacokinetics of Cefepime in Pediatric Intensive Care Patients",
      Authors = "An, et al.",
      Year = 2023,
      Journal = "Journal of Antimicrobial Chemotherapy",
      DOI = "10.1093/jac/dkad106",
      URL = "https://pubmed.ncbi.nlm.nih.gov/37071586/",
      Abstract = "This study evaluated the pharmacokinetics of cefepime in pediatric intensive care patients, identifying key covariates affecting drug clearance and supporting dosing regimen optimization.",
      Clearance_Formula = "$$ CL = 0.65 \\times \\left(\\frac{WT}{70}\\right)^{0.75} \\; (L/h) $$",
      Model_Description = "A population pharmacokinetic model that integrates weight and renal function parameters to predict cefepime clearance.",
      Population_Studied = "Pediatric intensive care patients"
    ),
    Barreto_2023 = tibble::tibble(
      Title = "Title placeholder for Barreto et al., AAC 2023",
      Authors = "Barreto, et al.",
      Year = 2023,
      Journal = "Antimicrobial Agents and Chemotherapy",
      DOI = NA,
      URL = "https://pubmed.ncbi.nlm.nih.gov/37882514/",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    )
  ),
  "meropenem" = list(
    # Added references for meropenem studies:
    Gijsen_2021 = tibble::tibble(
      Title = "Title placeholder for Gijsen et al., IDR 2021",
      Authors = "Gijsen, et al.",
      Year = 2021,
      Journal = "Infectious Diseases Reports",
      DOI = NA,
      URL = "https://pmc.ncbi.nlm.nih.gov/articles/PMC8754504/",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    ),
    Minichmayr_2018 = tibble::tibble(
      Title = "Title placeholder for Minichmayr et al., JAC 2018",
      Authors = "Minichmayr, et al.",
      Year = 2018,
      Journal = "Journal of Antimicrobial Chemotherapy",
      DOI = NA,
      URL = "https://pubmed.ncbi.nlm.nih.gov/29425283/",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    ),
    Ehrmann_2019 = tibble::tibble(
      Title = "Title placeholder for Ehrmann et al., IJAA 2019",
      Authors = "Ehrmann, et al.",
      Year = 2019,
      Journal = "International Journal of Antimicrobial Agents",
      DOI = NA,
      URL = "https://pmc.ncbi.nlm.nih.gov/articles/PMC9951903/",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    ),
    Huang_2025 = tibble::tibble(
      Title = "Title placeholder for Huang et al., 2025",
      Authors = "Huang, et al.",
      Year = 2025,
      Journal = "Journal placeholder",
      DOI = NA,
      URL = "URL not provided",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    ),
    Fukumoto_2023 = tibble::tibble(
      Title = "Population Pharmacokinetic Model and Dosing Simulation of Meropenem Using Measured Creatinine Clearance for Patients with Sepsis",
      Authors = "Fukumoto, et al.",
      Year = 2023,
      Journal = "Ther Drug Monit",
      DOI = "10.1097/FTD.0000000000001040",
      URL = "https://pubmed.ncbi.nlm.nih.gov/36253888/",
      Abstract = "<br> <strong> <i> Purpose :     </i> </strong> Creatinine clearance (CCr) and pharmacokinetic parameters are markedly affected by pathophysiological changes in patients with sepsis. However, only a few reports have assessed renal function in patients with sepsis using the measured CCr. Furthermore, the administration regimen has not been sufficiently evaluated using a population PK (PPK) model across renal function broad ranges. Therefore, this study was performed to construct a meropenem PPK model for patients with sepsis using the measured CCr and evaluate the optimized meropenem dosing regimen based on the CCr.
                  <br> <strong> <i> Methods :     </i> </strong> Patients with sepsis who received intravenous meropenem at the Showa University Hospital were enrolled in this prospective observational study. The PPK model was constructed using blood samples and clinical information of patients. The probability of target attainment (PTA) indicates the likelihood of achieving 50% time above the minimum inhibitory concentration (% T > MIC) based on 10,000 virtual patients using Monte Carlo simulations. The PTA for each meropenem regimen was 50% T > MIC based on different renal functions using the Monte Carlo simulation.
                  <br> <strong> <i> Results :     </i> </strong> One hundred samples were collected from 31 patients. The final PPK model incorporating the measured CCr as a covariate in CL displayed the best fit. The recommended dosing regimen to achieve a PTA of 50% T > MIC of 4 mcg/mL was 1 g every 8 hours as a 3-hour prolonged infusion for patients with CCr 85-130 mL/min and 1 g every 8 hours as an 8-hour continuous infusion for patients with CCr ≥ 130 mL/min.
                  <br> <strong> <i> Conclusions : </i> </strong> This model precisely predicted meropenem concentrations in patients with sepsis by accurately evaluating renal function using the measured CCr. Extended dosing was demonstrated to be necessary to achieve a PTA of 50% T > MIC for patients with CCr ≥ 85 mL/min. Meropenem effectiveness can be maximized in patients with sepsis by selecting the appropriate dosing regimen based on renal function and the MIC.",
      Clearance_Formula = "$$CL = 13.5 \\times \\left( \\frac{CCR}{87.6} \\right) ^ {0.67} \\qquad \\qquad With \\quad CCR = Urine Output \\times \\frac{Urine Cr}{SCr} \\times \\frac{1.73m²}{BSA} $$",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Intensive Care sepsis patients"
    ),
    Lan_2022 = tibble::tibble(
      Title = "Title placeholder for Lan et al., JPS 2022",
      Authors = "Lan, et al.",
      Year = 2022,
      Journal = "Journal of Pharmaceutical Sciences",
      DOI = NA,
      URL = "https://pubmed.ncbi.nlm.nih.gov/35090867/",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    )
  ),
  "ceftazidime" = list(
    # Added references for ceftazidime studies:
    Buning_2021 = tibble::tibble(
      Title = "Title placeholder for Buning et al., Antibiotics 2021",
      Authors = "Buning, et al.",
      Year = 2021,
      Journal = "Antibiotics",
      DOI = NA,
      URL = "https://www.mdpi.com/2079-6382/10/6/612",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    ),
    Launay_2024 = tibble::tibble(
      Title = "Title placeholder for Launay et al., Antibiotics 2024",
      Authors = "Launay, et al.",
      Year = 2024,
      Journal = "Antibiotics",
      DOI = NA,
      URL = "https://www.mdpi.com/2079-6382/13/8/756",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    ),
    Cojutti_2024 = tibble::tibble(
      Title = "Title placeholder for Cojutti et al., JAP 2024",
      Authors = "Cojutti, et al.",
      Year = 2024,
      Journal = "Journal of Antimicrobial Pharmacology",
      DOI = NA,
      URL = "https://pubmed.ncbi.nlm.nih.gov/39159014/",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    )
  ),
  "ceftolozane" = list(
    # Added references for ceftolozane studies:
    Chandorkar_2015 = tibble::tibble(
      Title = "Title placeholder for Chandorkar et al., ACCP 2015",
      Authors = "Chandorkar, et al.",
      Year = 2015,
      Journal = "ACCP Journal",
      DOI = NA,
      URL = "https://pubmed.ncbi.nlm.nih.gov/25196976/",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    ),
    Zhang_2021 = tibble::tibble(
      Title = "Title placeholder for Zhang et al., ACCP 2021",
      Authors = "Zhang, et al.",
      Year = 2021,
      Journal = "Critical Care",
      DOI = NA,
      URL = "https://ccforum.biomedcentral.com/articles/10.1186/s13054-021-03773-5",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    )
  )
)
