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
      Abstract = "This study presents a pharmacokinetic model of piperacillin-tazobactam in critically ill patients. The model describes the pharmacokinetics of both piperacillin and tazobactam, taking into account factors such as renal function and body weight.",
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
      Title = "Title placeholder for Fukumoto et al., 2023",
      Authors = "Fukumoto, et al.",
      Year = 2023,
      Journal = "Journal placeholder",
      DOI = "10.1097/FTD.0000000000001040",
      URL = "https://pubmed.ncbi.nlm.nih.gov/36253888/",
      Abstract = "Abstract placeholder.",
      Clearance_Formula = "Formula placeholder",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
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
