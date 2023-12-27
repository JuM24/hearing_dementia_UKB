# No evidence for a protective role of hearing aids for the risk of dementia in UK Biobank: a response to Jiang et al. (2023)

This repo contains the code for the manuscript and supplementary material that are also contained in this repo. We use UK Biobank data to compute two measures: (1) the HR of people with hearing loss who do not use hearing aids compared to people without hearing loss and (2) the HR of people with hearing loss who do use hearing aids compared to people without hearing loss. The purpose of this analysis was to reproduce findings by Jiang et al. (2023; doi:10.1016/S2468-2667(23)00048-8; now RETRACTED) that found (1) to be significantly above HR=1 and (2) to not be significantly above HR=1.The findings of the original paper indicated that people with hearing loss who use hearing aids do not exhibit the higher rates of dementia compared to people without hearing loss; and that people with hearing loss who do not use hearing aids do exhibit higher rates of dementia compared to people without hearing loss. This suggested a possibility for hearing aids to be protective against dementia; in an area where cross-sectional studies prevail and RCTs exhibit relatively short follow-ups, the study by Jiang et al. (2023) was considered an important contribution based on longitudinal data. We found that the results were erroneous, hence the retraction. Our manuscript was originally intended as a comment paper and submited to the Lancet Public Health (the same journal that had published the original false findings), but was rejected. Since the original study has since been retracted and there is no record of the corrected findings, we decided to upload the latter here.  

"inpatient_cleaning.R" combines inpatient diagnoses and dates of admissions into a single file. Its output is used in "Jiang_preparation.R." "Jiang_preparation.R": this imports all the variables and prepares a masterfile for analysis. "Jiang_analysis.R": this analyses the data. 

Versions of software used for the analyses:
R 4.3.1, 
data.table 1.14.8, 
gsubfn 0.7, 
marginaleffects 0.14.0, 
survival 3.5.5, 
tidyverse 2.0.0, 
zoo 1.8.11.
