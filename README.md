# No evidence for a protective role of hearing aids for the risk of dementia in UK Biobank: a response to Jiang et al. (2023)
Code to reproduce the main results by Jiang et al. (2023); doi:10.1016/S2468-2667(23)00048-8

Jiang_preparation.R: this imports all the variables and prepares a masterfile for analysis. Jiang_analysis.R: this analyses the data. inpatient_cleaning.R combines inpatient diagnoses and dates of admissions into a single file. Its output is used in Jiang_preparation.R.

Versions of software used for the analyses:
R 4.3.1, 
data.table 1.14.8, 
gsubfn 0.7, 
marginaleffects 0.14.0, 
survival 3.5.5, 
tidyverse 2.0.0, 
zoo 1.8.11, 
