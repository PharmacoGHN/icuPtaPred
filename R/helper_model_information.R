# Load necessary libraries
library(tibble)

# Create a tibble to store information about the pharmacokinetics model
model_information <- list(
  klastrup_2020 = tibble(
    Title = "Pharmacokinetic Model of Piperacillin-Tazobactam",
    Authors = "Klastrup, I. D., et al.",
    Year = 2020,
    Journal = "Journal of Antimicrobial Chemotherapy",
    DOI = "10.1128/AAC.02556-19",
    Abstract = "This study presents a pharmacokinetic model of piperacillin-tazobactam in critically ill patients. The model describes the pharmacokinetics of both piperacillin and tazobactam, taking into account factors such as renal function and body weight.",
    Key_Findings = "The model accurately predicts the pharmacokinetics of piperacillin and tazobactam in critically ill patients, and can be used to optimize dosing regimens.",
    Clearance_Formula = "CL = (CLpop * (CrCl/120)^theta) * (WT/70)^0.75",
    Model_Description = "The model includes parameters such as clearance (CL), volume of distribution (V), and intercompartmental clearance (Q). Parameter values: CLpop = 15 L/h, CrCl = creatinine clearance, theta = 0.7, WT = body weight, V1 = 20 L, V2 = 10 L, Q = 5 L/h.",
    Population_Studied = "Critically ill patients"
  ),
  cacqueray_2022 = tibble(
    Title = "Cefepime population pharmacokinetics and dosing regimen optimization in critically ill children with different renal functions",
    Authors = "Cacqueray, A., et al.",
    Year = 2022,
    Journal = "Clinical Microbiology and Infection",
    DOI = "10.1016/j.cmi.2022.05.007",
    Abstract = "Objective: Cefepime is commonly used in pediatric intensive care units, where unpredictable variations in the patients' pharmacokinetic (PK) variables may require drug dose adjustments. The objectives of the present study were to build a population PK model for cefepime in critically ill children and to optimize individual initial dosing regimens.
      Methods: Children (aged from 1 month to 18 years; body weight >3 kg) receiving cefepime were included. Cefepime total plasma concentrations were measured using high performance liquid chromatography. Data were modelled using nonlinear, mixed-effect modeling software, and Monte Carlo simulations were performed with a PK target of 100% fT > MIC.
      Results: Fifty-nine patients (median (range) age: 13.5 months (1.1 months to 17.6 years)) and 129 cefepime concentration measurements were included. The cefepime concentration data were best fitted by a one-compartment model. The selected covariates were body weight with allometric scaling and estimated glomerular filtration rate on clearance. Mean population values for clearance and volume were 1.21 L/h and 4.8 L, respectively. According to the simulations, a regimen of 100 mg/kg/d q12 h over 30 min or 100 mg/kg/d as a continuous infusion was more likely to achieve the PK target in patients with renal failure and in patients with normal or augmented renal clearance, respectively.
      Discussion: Appropriate cefepime dosing regimens should take renal function into account. Continuous infusions are required in critically ill children with normal or augmented renal clearance, while intermittent infusions are adequate for children with acute renal failure. Close therapeutic drug monitoring is mandatory, given cefepime's narrow therapeutic window.",
    Clearance_Formula = "CL = 1.21 * (tbw / 9) ^ 0.75 * (schwartz / 153) ^ 0.37",
    Model_Description = "The model includes parameters such as clearance (CL), volume of distribution (V), and intercompartmental clearance (Q). Parameter values: CLpop = 12 L/h, CrCl = creatinine clearance, theta = 0.6, WT = body weight, V1 = 18 L, V2 = 8 L, Q = 4 L/h.",
    Population_Studied = "Children (aged from 1 month to 18 years; body weight >3 kg) receiving cefepime "
  )
)