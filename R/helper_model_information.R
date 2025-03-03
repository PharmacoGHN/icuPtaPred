#' @importFrom tibble tibble

# Create a tibble to store information about the pharmacokinetics model

model_information <- list(
  "Piperacillin-tazobactam" = list(
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
      Clearance_Formula = "$$ CL = 2.25 + 0.119  \\times CG_{TBW} $$",
      Model_Description = "non renal clearance = 2.25 L/h, renal clearance = 0.119 x CRCL calculated with Cockcroft and Gault using the Total Body Weight (TBW).",
      Population_Studied = "Critically ill patients"
    ),
    # Added references for Piperacillin-tazobactam studies:
    Sukarnjanaset_2019 = tibble::tibble(
      Title = "Population pharmacokinetics and pharmacodynamics of piperacillin in critically ill patients during the early phase of sepsis",
      Authors = "Sukarnjanaset, et al.",
      Year = 2019,
      Journal = "Journal of Pharmacokinetics and Pharmacodynamics",
      DOI = "10.1007/s10928-019-09633-8",
      URL = "https://pubmed.ncbi.nlm.nih.gov/30963365/",
      Abstract = "
      <br> <strong> Objectives : </strong> This study aimed to characterize the population pharmacokinetics (PKs) of piperacillin and investigate probability of
        target attainment (PTA) and cumulative fraction of response (CFR) of various dosage regimens in critically ill patients during the early phase of sepsis.
      <br> <strong> Methods : </strong>  Forty-eight patients treated with piperacillin/tazobactam were recruited. 
        Five blood samples were drawn before and during 0-0.5, 0.5-2, 2-4 and 4-6 or 8 h after administration. Population PKs was analyzed using NONMEM®.
        The PTA of 90%fT>MIC target and CFR were determined by Monte Carlo simulation.
      <br> <strong> Results : </strong> The two compartment model best described the data. Piperacillin clearance (CL) was 5.37 L/h, central volume of distribution
        (V1) was 9.35 L, and peripheral volume of distribution was 7.77 L. Creatinine clearance (CLCr) and mean arterial pressure had a significant effect on CL
        while adjusted body weight had a significant impact on V1. Subtherapeutic concentrations can occur during the early phase of sepsis in critically ill patients with normal
        renal function. The usual dosage regimen, 4 g of piperacillin infused over 0.5 h every 6 h, could not achieve the target for susceptible organisms with MIC 16 mg/L
        in patients with CLCr ≥ 60 mL/min. Our proposed regimen for the patients with CLCr 60-120 mL/min was an extended 2 h infusion of 4 g of piperacillin every 6 h.
      <br> <strong> Conclusion : </strong> Most regimens provided CFR ≥ 90% for the E. coli infection while there was no dosage regimen
        achieved a CFR of 90% for the P. aeruginosa infection.
      ",
      Clearance_Formula = "$$ CL = 5.37 + \\left( 0.06 \\times (Cl_{CR} - 55)  \\right) + \\left( 0.05 + (MAP - 68) \\right) $$",
      Model_Description = "MAP = Mean arterial pressure; CL = Clearance; CLCr = Creatinine clearance.
      <br> MAP parameters is currently disabled from CL calculation",
      Population_Studied = "Critically ill patients; Pharmacodynamics; Piperacillin; Population pharmacokinetics; Sepsis; β-Lactams."
    ),
    Udy_2015 = tibble::tibble(
      Title = "Are standard doses of piperacillin sufficient for critically ill patients with augmented creatinine clearance?",
      Authors = "Udy, et al.",
      Year = 2015,
      Journal = "Critical Care",
      DOI = "10.1186/s13054-015-0750-y",
      URL = "https://doi.org/10.1186/s13054-015-0750-y",
      Abstract = "
      <br> <strong> <i> Introduction </i> </strong> The aim of this study was to explore the impact of augmented creatinine clearance and
        differing minimum inhibitory concentrations (MIC) on piperacillin pharmacokinetic/pharmacodynamic (PK/PD) target attainment (time above MIC (fT>MIC))
      in critically ill patients with sepsis receiving intermittent dosing.
      <br> <strong> <i> Methods </i> </strong> To be eligible for enrolment, critically ill patients with sepsis had to be receiving piperacillin-tazobactam 4.5 g
        intravenously (IV) by intermittent infusion every 6 hours for presumed or confirmed nosocomial infection without significant renal impairment (defined by a 
        plasma creatinine concentration greater than 171 μmol/L or the need for renal replacement therapy). Over a single dosing interval, blood samples were drawn 
        to determine unbound plasma piperacillin concentrations. Renal function was assessed by measuring creatinine clearance (CLCR). A population PK model was constructed,
        and the probability of target attainment (PTA) for 50% and 100% fT>MIC was calculated for varying MIC and CLCR values.
      <br> <strong> <i> Results </i> </strong> In total, 48 patients provided data. Increasing CLCR values were associated with lower trough plasma piperacillin concentrations
        (P < 0.01), such that with an MIC of 16 mg/L, 100% fT>MIC would be achieved in only one-third (n = 16) of patients. Mean piperacillin clearance was approximately 1.5-fold
        higher than in healthy volunteers and correlated with CLCR (r = 0.58, P < 0.01). A reduced PTA for all MIC values, when targeting either 50% or 100% fT>MIC,
        was noted with increasing CLCR measures.
      <br> <strong> <i> Conclusions </i> </strong> Standard intermittent piperacillin-tazobactam dosing is unlikely to achieve optimal piperacillin exposures in a significant
        proportion of critically ill patients with sepsis, owing to elevated drug clearance. These data suggest that CLCR can be employed as a useful tool to determine whether
        piperacillin PK/PD target attainment is likely with a range of MIC values.
      ",
      Clearance_Formula = "$$ CL = 16.3 \\times \\left( \\frac{CL_{CR}}{100} \\right) $$ ",
      Model_Description = " CL = Clearance; CLCR = Creatinine clearance.",
      Population_Studied = "Critically ill patients"
    )
  ),
  "Cefepime" = list(
    cacqueray_2022 = tibble::tibble(
      Title = "cefepime population pharmacokinetics and dosing regimen optimization in critically ill children with different renal functions",
      Authors = "Cacqueray, A., et al.",
      Year = 2022,
      Journal = "Clinical Microbiology and Infection",
      DOI = "10.1016/j.cmi.2022.05.007",
      URL = "https://pubmed.ncbi.nlm.nih.gov/35605841/",
      Abstract = "
      Objective: cefepime is commonly used in pediatric intensive care units, where unpredictable variations in the patients' pharmacokinetic (PK) variables may require drug dose adjustments. 
      <br> Methods: Children (aged from 1 month to 18 years; body weight >3 kg) receiving cefepime were included... ",
      Clearance_Formula = "$$\nCL = 1.21 * \\left(\\frac{TBW}{9}\\right)^{0.75} * \\left(\\frac{CLCR_{Schwartz}}{153}\\right)^{0.37}\n$$",
      Model_Description = " TBW = Total body weight; Schwartz = Schwartz formula for creatinine clearance.",
      Population_Studied = "Children (aged from 1 month to 18 years; body weight >3 kg) receiving cefepime"
    ),
    # Added references for Cefepime studies:
    An_2023 = tibble::tibble(
      Title = "Population Pharmacokinetics of cefepime in Pediatric Intensive Care Patients",
      Authors = "An, et al.",
      Year = 2023,
      Journal = "Journal of Antimicrobial Chemotherapy",
      DOI = "10.1093/jac/dkad106",
      URL = "https://pubmed.ncbi.nlm.nih.gov/37071586/",
      Abstract = "
      <br> <strong> Objectives: </strong> We aimed to identify rational empirical dosing strategies for cefepime treatment in critically ill patients by utilizing
      population pharmacokinetics and target attainment analysis.
      <br> <strong> Patients and methods: </strong> A prospective and opportunistic pharmacokinetic (PK) study was conducted in 130 critically ill patients
      in two ICU sites. The plasma concentrations of cefepime were determined using a validated LC-MS/MS method. All cefepime PK data were analysed
      simultaneously using the non-linear mixed-effects modelling approach. Monte Carlo simulations were performed to evaluate the PTA of cefepime at different
      MIC values following different dose regimens in subjects with different renal functions.
      <br> <strong> Results: </strong> The PK of cefepime in critically ill patients was best characterized by a two-compartment model with zero-order input
      and first-order elimination. Creatinine clearance and body weight were identified to be significant covariates.
      Our simulation results showed that prolonged 3 h infusion does not provide significant improvement on target attainment compared with the traditional
      intermittent 0.5 h infusion. In contrast, for a given daily dose continuous infusion provided much higher breakpoint coverage than either 0.5h or 3h intermittent infusions.
      To balance the target attainment and potential neurotoxicity, cefepime 3 g/day continuous infusion appears to be a better dosing regimen than 6 g/day continuous infusion.
      <br> <strong> Conclusions: </strong> Continuous infusion may represent a promising strategy for cefepime treatment in critically ill patients. With the availability of
      institution- and/or unit-specific cefepime susceptibility patterns as well as individual patients' renal function, our PTA results may represent useful
      references for physicians to make dosing decisions.
      ",
      Clearance_Formula = "$$ CL (L/h) =  2 \\times \\left( \\frac{CL_{CR, LBW}}{54} \\right)^{0.75} $$",
      Model_Description = " CL = Clearance; CLCR = Creatinine clearance; LBW = Lean body weight.",
      Population_Studied = "Pediatric intensive care patients"
    ),
    Barreto_2023 = tibble::tibble(
      Title = "Population pharmacokinetic model of cefepime for critically ill adults: a comparative assessment of eGFR equations",
      Authors = "Barreto, et al.",
      Year = 2023,
      Journal = "Antimicrobial Agents and Chemotherapy",
      DOI = "10.1128/aac.00810-23",
      URL = "https://pubmed.ncbi.nlm.nih.gov/37882514/",
      Abstract = "
      <br> cefepime exhibits highly variable pharmacokinetics in critically ill patients. The purpose of this study was to develop and qualify a population
      pharmacokinetic model for use in the critically ill and investigate the impact of various estimated glomerular filtration rate (eGFR) equations
      using creatinine, cystatin C, or both on model parameters. 
      <br> <strong> Methods : </strong> This was a prospective study of critically ill adults hospitalized at an academic medical center treated with intravenous cefepime.
      Individuals with acute kidney injury or on kidney replacement therapy or extracorporeal membrane oxygenation were excluded. A nonlinear mixed-effects population 
      pharmacokinetic model was developed using data collected from 2018 to 2022.
      <br> <strong> Results </strong> The 120 included individuals contributed 379 serum samples for analysis. A two-compartment pharmacokinetic model with first-order 
      elimination best described the data. The population mean parameters (standard error) in the final model were 7.84 (0.24) L/h for CL1 and 15.6 (1.45) L for V1. 
      Q was fixed at 7.09 L/h and V2 was fixed at 10.6 L, due to low observed interindividual variation in these parameters. The final model included weight as a covariate 
      for volume of distribution and the eGFRcr-cysC (mL/min) as a predictor of drug clearance. 
      <br> <strong> Conclusion </strong> In summary, a population pharmacokinetic model for cefepime was created for critically ill adults. The study demonstrated the importance
      of cystatin C to prediction of cefepime clearance. cefepime dosing models which use an eGFR equation inclusive of cystatin C are likely to exhibit improved accuracy and
      precision compared to dosing models which incorporate an eGFR equation with only creatinine.",
      Clearance_Formula = "
      $$ CL = 7.84 \\times \\frac{eGFR_{cr-cysC}}{7.2} $$
      ",
      Model_Description = " CL = Clearance; eGFR = estimated glomerular filtration rate; cr = creatinine; cysC = cystatin C.",
      Population_Studied = "Critically ill Adults"
    )
  ),
  "Meropenem" = list(
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
      Abstract = "
      <br> <strong> <i> Purpose :     </i> </strong> Creatinine clearance (CCr) and pharmacokinetic parameters are markedly affected by pathophysiological
        changes in patients with sepsis. However, only a few reports have assessed renal function in patients with sepsis using the measured CCr. Furthermore,
        the administration regimen has not been sufficiently evaluated using a population PK (PPK) model across renal function broad ranges. Therefore, this study was
        performed to construct a meropenem PPK model for patients with sepsis using the measured CCr and evaluate the optimized meropenem dosing regimen based on the CCr.
      <br> <strong> <i> Methods :     </i> </strong> Patients with sepsis who received intravenous meropenem at the Showa University Hospital were enrolled in this 
        prospective observational study. The PPK model was constructed using blood samples and clinical information of patients. The probability of target attainment (PTA)
        indicates the likelihood of achieving 50% time above the minimum inhibitory concentration (% T > MIC) based on 10,000 virtual patients using Monte Carlo simulations.
        The PTA for each meropenem regimen was 50% T > MIC based on different renal functions using the Monte Carlo simulation.
      <br> <strong> <i> Results :     </i> </strong> One hundred samples were collected from 31 patients. The final PPK model incorporating the measured CCr as a covariate in CL
        displayed the best fit. The recommended dosing regimen to achieve a PTA of 50% T > MIC of 4 mcg/mL was 1 g every 8 hours as a 3-hour prolonged infusion for patients with
        CCr 85-130 mL/min and 1 g every 8 hours as an 8-hour continuous infusion for patients with CCr ≥ 130 mL/min.
      <br> <strong> <i> Conclusions : </i> </strong> This model precisely predicted meropenem concentrations in patients with sepsis by accurately evaluating renal
        function using the measured CCr. Extended dosing was demonstrated to be necessary to achieve a PTA of 50% T > MIC for patients with CCr ≥ 85 mL/min. Meropenem effectiveness
        can be maximized in patients with sepsis by selecting the appropriate dosing regimen based on renal function and the MIC.",
      Clearance_Formula = "$$CL = 13.5 \\times \\left( \\frac{CCR}{87.6} \\right) ^ {0.67} \\qquad \\qquad With \\quad CCR = Urine Output \\times \\frac{Urine Cr}{SCr} \\times \\frac{1.73m^2}{BSA} $$",
      Model_Description = " CL = Clearance; CCr = Creatinine clearance; SCr = Serum creatinine; BSA = Body surface area.",
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
  "Ceftazidime" = list(
    # Added references for ceftazidime studies:
    Buning_2021 = tibble::tibble(
      Title = "Population Pharmacokinetics and Probability of Target Attainment of Different Dosing Regimens of Ceftazidime in Critically Ill Patients with a Proven or Suspected Pseudomonas aeruginosa Infection",
      Authors = "Buning, et al.",
      Year = 2021,
      Journal = "Antibiotics",
      DOI = "10.3390/antibiotics10060612",
      URL = "https://www.mdpi.com/2079-6382/10/6/612",
      Abstract = "
      <br> <strong> Objectives : </strong> Altered pharmacokinetics (PK) of hydrophilic antibiotics in critically ill patients is common, with possible consequences for
        efficacy and resistance. We aimed to describe ceftazidime population PK in critically ill patients with a proven or suspected Pseudomonas aeruginosa
        infection and to establish optimal dosing.
      <br> <strong> Methods : </strong> Blood samples were collected for ceftazidime concentration measurement. A population PK model was constructed, and probability of target
        attainment (PTA) was assessed for targets 100% T > MIC and 100% T > 4 × MIC in the first 24 h. 
      <br> <strong> Results : </strong> Ninety-six patients yielded 368 ceftazidime concentrations. In a one-compartment model, variability in ceftazidime clearance (CL) showed
        association with CVVH. For patients not receiving CVVH, variability in ceftazidime CL was 103.4% and showed positive associations with creatinine clearance and with the
        comorbidities hematologic malignancy, trauma or head injury, explaining 65.2% of variability. For patients treated for at least 24 h and assuming a worst-case MIC of 8 
        mg/L, PTA was 77% for 100% T > MIC and 14% for 100% T > 4 × MIC. Patients receiving loading doses before continuous infusion demonstrated higher PTA than patients who did
        not (100% T > MIC: 95% (n = 65) vs. 13% (n = 15); p < 0.001 and 100% T > 4 × MIC: 20% vs. 0%; p = 0.058). The considerable IIV in ceftazidime PK in ICU patients could
        largely be explained by renal function, CVVH use and several comorbidities. 
      <br> <strong> Conclusion : </strong> Critically ill patients are at risk for underexposure to ceftazidime when empirically aiming
        for the breakpoint MIC for P. aeruginosa. A loading dose is recommended.",
      Clearance_Formula = "$$ CL_{nonCVVH} = 3.42 \\times \\left( \\frac{CKDEPI_i}{73} \\right)^{0.72} \\times 1.57^{hemat} \\times 2^{trauma} $$",
      Model_Description = "CKD-EPI = Chronic Kidney Disease Epidemiology Collaboration; hemat = hematologic malignancy; trauma = trauma or head injury.
       <br> <strong> Trauma and Hematology malignancy not supported at the moment </strong>",
      Population_Studied = "Critically ill patients, CVVH, trauma, hematologic malignancy"
    ),
    Launay_2024 = tibble::tibble(
      Title = "Loading Dose of Ceftazidime Needs to Be Increased in Critically Ill Patients: A Retrospective Study to Evaluate Recommended Loading Dose with Pharmacokinetic Modelling",
      Authors = "Launay, et al.",
      Year = 2024,
      Journal = "Antibiotics",
      DOI = "10.3390/antibiotics13080756",
      URL = "https://www.mdpi.com/2079-6382/13/8/756",
      Abstract = "
      <br> <strong> Objectivs </strong> To rapidly achieve ceftazidime target concentrations, a 2 g loading dose (LD) is recommended before continuous infusion, but its adequacy
        in critically ill patients, given their unique pharmacokinetics, needs investigation. This study included patients from six ICUs in Saint-Etienne and Paris,
        France, who received continuous ceftazidime infusion with plasma concentration measurements.
      <br> <strong> Methods </strong>  Using MONOLIX and R, a pharmacokinetic (PK) model was developed, and the literature on ICU patient PK models was reviewed. Simulations
        calculated the LD needed to reach a 60 mg/L target concentration and assessed ceftazidime exposure for various regimens.
      <br> <strong> </strong> Among 86 patients with 223 samples, ceftazidime PK was best described by a one-compartment model with glomerular filtration rate explaining clearance
        variability. Typical clearance and volume of distribution were 4.45 L/h and 88 L, respectively. The literature median volume of distribution was 37.2 L. Simulations
        indicated that a Loading dose higher than 2 g was needed to achieve 60 mg/L in 80% of patients, with a median LD of 4.9 g. 
      <br> <strong> </strong> Our model showed a 4 g LD followed by 6 g/day infusion reached effective concentrations within 1 h, while a 2 g LD caused an 18 h delay
        in achieving target steady state.",
      Clearance_Formula = "$$ CL = 4.45 \\times \\left( \\frac{CKDEPI_i}{73.9} \\right)^{0.9} $$",
      Model_Description = "CKD-EPI = Chronic Kidney Disease Epidemiology Collaboration, 2009 formula is used.",
      Population_Studied = "Critically ill patients, continuous infusion"
    ),
    Cojutti_2024 = tibble::tibble(
      Title = "
        An innovative population pharmacokinetic/pharmacodynamic strategy for attaining aggressive joint PK/PD target of continuous infusion
        ceftazidime/avibactam against KPC- and OXA-48- producing Enterobacterales and preventing resistance development in critically ill patients",
      Authors = "Cojutti, et al.",
      Year = 2024,
      Journal = "Journal of Antimicrobial Pharmacology",
      DOI = "10.1093/jac/dkae290",
      URL = "https://pubmed.ncbi.nlm.nih.gov/39159014/",
      Abstract = "
      <br> <strong> Objectives: </strong> Ceftazidime/avibactam is a key antibiotic for carbapenemase-producing Enterobacterales (CPE) Gram-negative infections, but
        current dosing may be suboptimal to grant activity. This study explores the population pharmacokinetics/pharmacodynamics (PK/PD) of continuous infusion (CI)
        ceftazidime/avibactam for maximizing treatment efficacy in critically ill patients.
      <br> <strong> Methods: </strong> A retrospective analysis of adult patients receiving CI ceftazidime/avibactam and therapeutic drug monitoring (TDM) of both
        compounds was performed. Population PK/PD modelling identified the most accurate method for estimating ceftazidime/avibactam clearance based on kidney function
        and Monte Carlo simulations investigated the relationship between various CI dosing regimens and aggressive joint PK/PD target attainment of ceftazidime/avibactam.
      <br> <strong> Results: </strong> The European Kidney Function Consortium (EKFC) equation best described kidney function for ceftazidime/avibactam clearance.
        The findings challenge the current approach of only reducing the ceftazidime/avibactam dose based on kidney function by identifying dose adjustments in patients
        with augmented kidney function. Our CI ceftazidime/avibactam dosing strategies, adjusted by TDM, showed promise for achieving optimal aggressive joint PK/PD
        targets and potentially improving clinical/microbiological outcomes against KPC- and OXA-48-producing Enterobacterales. The risk of neurotoxicity associated with
        these strategies appears acceptable.
      <br> <strong> Conclusions: </strong> This study suggests that adjusting ceftazidime/avibactam dosing regimen based solely on eCLcr might be suboptimal for critically
        ill patients. Higher daily doses delivered by CI and adjusted based on TDM have the potential to improve aggressive joint PK/PD target attainment and potentially
        clinical/microbiological outcomes. Further investigations are warranted to confirm these findings and establish optimal TDM-guided dosing strategies for
        ceftazidime/avibactam in clinical practice.",
      Clearance_Formula = "$$ CL_{caz} = 5 \\times \\frac{EKFC}{70}^{0.7} $$",
      Model_Description = "Model description placeholder.",
      Population_Studied = "Population placeholder"
    )
  ),
  "Ceftolozane" = list(
    # Added references for ceftolozane studies:
    Chandorkar_2015 = tibble::tibble(
      Title = "Title placeholder for Chandorkar et al., ACCP 2015",
      Authors = "Chandorkar, et al.",
      Year = 2015,
      Journal = "ACCP Journal",
      DOI = "10.1002/jcph.395",
      URL = "https://pubmed.ncbi.nlm.nih.gov/25196976/",
      Abstract = "
      <br> <strong> Background : </strong> Ceftolozane/tazobactam is a novel antipseudomonal cephalosporin and β-lactamase inhibitor in clinical development for treatment of
       complicated urinary tract (cUTI) and intra-abdominal (cIAI) infections and nosocomial pneumonia. 
      <br> <strong> Methods : </strong> The population pharmacokinetics of ceftolozane/tazobactam were characterized in healthy volunteers, subjects with varying degrees
        of renal function, and patients with cIAI or cUTI. Serum concentration data from 376 adults who received ceftolozane/tazobactam
        in doses ranging from 500 to 3000 mg were analyzed to identify factors contributing to the pharmacokinetic variability.
      <br> <strong> Results : </strong> Ceftolozane/tazobactam pharmacokinetics were well described by a linear two-compartment model with first-order elimination
        and moderate between-subject variability in both clearance and volume of distribution (Vc). For both ceftolozane and tazobactam,
        clearance was highly correlated with renal function with creatinine clearance influencing exposure, and infection influencing Vc.
        Body weight was an additional covariate affecting the Vc of ceftolozane. Other covariates tested, such as age, body weight, sex,
        ethnicity, and presence of infection, had no clinically relevant effects on exposure.       
      <br> <strong> Conclusion </strong> The final pharmacokinetic models adequately described the plasma concentrations of ceftolozane
       and tazobactam and form the basis for further modeling and simulation including evaluation of probability of target attainment
       in a diverse population with varying demographics, degrees of renal function, and infection status.",
      Clearance_Formula = "$$ CL = 5.11 \\times \\left( \\frac{CL_{CR}}{109} \\right)^{0.715} \\times 1.22^{infection} $$",
      Model_Description = "CL = Clearance; CLCR = Creatinine clearance; infection = presence of infection.",
      Population_Studied = "Healthy volunteers, subjects with varying degrees of renal function, and patients with cIAI or cUTI"
    ),
    Zhang_2021 = tibble::tibble(
      Title = "Ceftolozane/tazobactam probability of target attainment and outcomes in participants with augmented renal clearance from the randomized phase 3 ASPECT-NP trial",
      Authors = "Zhang, et al.",
      Year = 2021,
      Journal = "J Clin Pharmacol",
      DOI = "10.1002/jcph.1733",
      URL = "https://accp1.onlinelibrary.wiley.com/doi/10.1002/jcph.1733",
      Abstract = "
        <br> <strong> Background : </strong> Ceftolozane/tazobactam (C/T) is a combination of a novel cephalosporin with
          tazobactam, recently approved for the treatment of hospital-acquired and ventilator-associated pneumonia. The plasma
          pharmacokinetics (PK) of a 3-g dose of C/T (2 g ceftolozane and 1 g tazobactam) administered via a 1-hour infusion every 8 hours in adult
          patients with nosocomial pneumonia (NP) were evaluated in a phase 3 study (ASPECT-NP; NCT02070757). The present work describes the development
          of population PK models for ceftolozane and tazobactam in plasma and pulmonary epithelial lining fluid (ELF). 
        <br> <strong> Methods/ Results : </strong> The concentration-time profiles of both agents were well characterized by 2-compartment models with
          zero-order input and first-order elimination. Consistent with the elimination pathway, renal function estimated by creatinine clearance significantly
          affected the clearance of ceftolozane and tazobactam. The central volumes of distribution for both agents and the peripheral volume of distribution
          for tazobactam were approximately 2-fold higher in patients with pneumonia compared with healthy participants. A hypothetical link model was developed
          to describe ceftolozane and tazobactam disposition in ELF in healthy participants and patients with pneumonia. Influx (from plasma to the ELF compartment)
          and elimination (from the ELF compartment) rate constants were approximately 97% lower for ceftolozane and 52% lower for tazobactam in patients with
          pneumonia versus healthy participants. 
        <br> <strong> Conclusion : </strong> These population PK models adequately described the plasma and ELF concentrations of ceftolozane and 
          tazobactam, thus providing a foundation for further modeling and simulation, including the probability of target attainment assessments 
          to support dose recommendations of C/T in adult patients with NP.
        ",
      Clearance_Formula = "$$ CL = 4.84 \\times \\left( \\frac{CR_{CL}}{100} \\right)^{0.701} \\times 1.18^{UTI} \\times 1.43^{cIAI} \\times 0.32^{ESRD} $$",
      Model_Description = "CL = Clearance; CRCL = Creatinine clearance; UTI = urinary tract infection;
      cIAI = complicated intra-abdominal infection; ESRD = end-stage renal disease.
      ESRD is currently disabled from CL calculation",
      Population_Studied = "Adult patients with nosocomial pneumonia"
    )
  )
)
