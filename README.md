<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![codecov](https://codecov.io/gh/PharmacoGHN/icuPtaPred/graph/badge.svg?token=KU5a47P0ek)](https://codecov.io/gh/PharmacoGHN/icuPtaPred)
[![R-CMD-check](https://github.com/PharmacoGHN/icuPtaPred/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PharmacoGHN/icuPtaPred/actions/workflows/R-CMD-check.yaml)
[![License: AGPL-3](https://img.shields.io/badge/license-AGPL--3-blue.svg)](https://cran.r-project.org/web/licenses/AGPL-3)
<!-- badges: end -->
## Package Content  

This tool is designed to help Physician, Pharmacologist, Pharmacist and other Healthcare professional adjust Continuous Infusion Beta-lactamin dosing.  
This is a shiny application designed to facilitate drug adaptation.  
The application is not hosted for the moment but will be available [here]()  


## Currently supported drug are :

- [x] Piperacillin-Tazobactam
  - [x] [Klastrup et al. JAC, 2020](https://journals.asm.org/doi/10.1128/aac.02556-19)
  - [x] [Sukarnjanaset et al, JPP 2019](https://pubmed.ncbi.nlm.nih.gov/30963365/ )
  - [x] [Udy et al, 2015](https://pubmed.ncbi.nlm.nih.gov/25632974/ ) 
- [ ] Cefepim
  - [x] [An et al JAC 2023](https://pubmed.ncbi.nlm.nih.gov/37071586/)
  - [ ] [Barreto et al, AAC 2023](https://pubmed.ncbi.nlm.nih.gov/37882514/)
- [x] Ceftazidim
  - [x] [Buning et al, Antibiotics, 2021](https://www.mdpi.com/2079-6382/10/6/612)
  - [x] [Launay et al, Antibiotics, 2024](https://www.mdpi.com/2079-6382/13/8/756)
  - [x] [Cojutti et al, JAC 2024](https://pubmed.ncbi.nlm.nih.gov/39159014/)
- [ ] Meropenem
   - [ ] [Gijsen et al, IDR, 2021](https://pmc.ncbi.nlm.nih.gov/articles/PMC8754504/)
   - [ ] [Minichmayr et al, JAC, 2018](https://pubmed.ncbi.nlm.nih.gov/29425283/)
   - [ ] [Ehrmann et al, IJAA, 2019](https://pmc.ncbi.nlm.nih.gov/articles/PMC9951903/)
   - [ ] [Huang et al, 2025]()
   - [x] [Fukumoto et al, 2023](https://pubmed.ncbi.nlm.nih.gov/36253888/)
   - [ ] [Lan et al, JPS, 2022](https://pubmed.ncbi.nlm.nih.gov/35090867/)
- [x] Ceftolozane
  - [x] [Chandorkar et al ACCP 2015](https://pubmed.ncbi.nlm.nih.gov/25196976/)
  - [x] [Zhang et al, ACCP, 2021](https://ccforum.biomedcentral.com/articles/10.1186/s13054-021-03773-5)



## Installation

```
install.packages("blicm-don_1.0.0.tar.gz")  
devtools::install_github("PharmacoGHN/blicm-don")
```

## Authors and acknowledgment
Actual contributor :

   - Dr. R. Garreau, Pharm.D, Ph.D - Owner and Main Maintainer  
   - Pr. A. Friggeri, M.D, Ph.D - Owner  
   - Pr. S. Goutelle, Pharm.D, Ph.D - Owner  
   - L. Leyenberger, Pharmacy Resident, Msc. - Owner

## License
General Copyright law applies

##Project status
Ongoing and active
