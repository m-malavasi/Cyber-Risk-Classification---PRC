
# Cyber Risk Classification — PRC

This repository contains R code used in the analysis presented in the paper:

**Cyber Risk Taxonomies: Statistical Analysis of Cybersecurity Risk Classifications**  
by Matteo Malavasi, Gareth W. Peters, Stefan Trueck, Pavel V. Shevchenko, Jiwook Jang and Georgy Sofronov


The code supports the statistical analysis of cybersecurity risk classifications using data from the [Privacy Rights Clearinghouse](https://privacyrights.org/data-breaches)




### `Frequency_Analysis_PRC.R`
This script performs frequency analysis of cyber risk events, following the methodology in [Malavasi (2022)](https://doi.org/10.1016/j.insodeling the severity and frequency of incidents using various classification schemes.

**Outputs:**
- `Threshold_PRC.csv`: Threshold values for Peak Over Threshold (POT) modeling.
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

   

   
