# Cyber-Risk-Classification---PRC

This repository contains code for part of the analysis presneted in the paper Cyber Risk Taxonomies: Statistical Analysis of Cybersecurity Risk Classifications.

The data required to run this code can be found at https://privacyrights.org/data-breaches


# R file for main analysis

Frequency_Analysis_PRC.R: this file creates the covariates used in the frequency analysis in the spirit of [Malavasi, 2022](https://doi.org/10.1016/j.insmatheco.2022.05.003). The output files are:
1. Threshold_PRC.csv: a .csv file with the values of the "high enough" threshold for POT
2. Covariates_Severity_PRC.csv: a file with the covariates corresponding to the severity of cyber risk events, extracted from the PRC data

The code also generates Frequency covaraites for each of the following classifications discussed in the paper:
1. Classification based on PRC risk types
2. Body Classificaiton
3. Tail Classification
4. Fre/Sev Classification
5. TYpe/Importance Classification
6. Random Classification

For more detailes on how these classificaiotn are consturcted please refr to the manuscript.

Two_Sample_Test_PRC_data.R: runs the two sample test discussed in the manuscript and produces results presented in Section 5 of the Suppelementary Material. 
   

   
