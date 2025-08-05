
# Cyber Risk Classification — PRC

This repository contains R code used in the analysis presented in the paper:

**Cyber Risk Taxonomies: Statistical Analysis of Cybersecurity Risk Classifications**  
by Matteo Malavasi, Gareth W. Peters, Stefan Trueck, Pavel V. Shevchenko, Jiwook Jang and Georgy Sofronov


## Data Source

The analysis uses breach data from the [Privacy Rights Clearinghouse](https://privacyrights.org/data-breaches) manually before running the scripts.




### `Frequency_Analysis_PRC.R`
This script performs frequency analysis of cyber risk events, following the methodology in [Malavasi (2022)](https://doi.org/10.1016/j. 

**Outputs:**
- `Threshold_PRC.csv`: Threshold values for POT modeling.
- `Covariates_Severity_PRC.csv`: Covariates related to the severity of cyber events.

**Classification Schemes:**
1. PRC Risk Types  
2. Body Classification  
3. Tail Classification  
4. Frequency/Severity (Fre/Sev) Classification  
5. Type/Importance Classification  
6. Random Classification  

Refer to the manuscript for detailed construction of these schemes.

---

### `Two_Sample_Test_PRC_data.R`
This script runs two-sample statistical tests comparing distributions across classification schemes. Results are presented in **Section 5 of the Supplementary Material** of the paper.




   
