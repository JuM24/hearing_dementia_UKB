# This code reads in and prepares all variables used for reproduction of the Jiang et al. (2023) paper. It reads in:
# participant_opt_out.csv: contains ids of those participants that have opted out of providing data to UKB - these must be removed
# jiang_data.csv: contains field IDs 21022, 31, 53, 21000, 6138, 845, 22189, 738, 20116, 1239, 1249, 1558, 22032, 884, 
  # 21001, 20002, 2443, 6177, 6153, 6150, 709, 1031, 6160, 2050, 2020, 3393, 2247, 42018, 42020, 42022, 130836, 131036,
  # 130838, 130840, 130842
# deprivation.csv: field ID 22189
# death_causes.csv: field ID 40001
# death_date.csv: field ID 40000
# inpatient_diagnoses.rds: a file that combines data field IDs 41270, 41271, 41280, and 41281,which refer to inpatient diagnoses 
  # and dates of admission; for 6 participants, array indexing fails when combining diagnoses codes and dates, so I used the 
  # record-level data to access hospital addmissions for those 6 participants
# hesin.txt: record-level inpatient data (category ID 2000)
# gp_clinical.txt: primary care diagnoses (field ID 42040)
# apoe.csv: data for the SNPs rs429358 and rs7412 used to genotype APOE



library(tidyverse)


# remove those participants that opted out of the study
opt_outs <- read.csv('participant_opt_out.csv')

# read in the dataset
hear <- read.csv('jiang_data.csv') %>%
  rename(id = eid) %>%
  filter(!id %in% opt_outs$id)

# extract columns that match the pattern (start of line, 'X', digit, '.0.')
cols_to_keep <- grep("X\\d+\\.0\\.", names(hear), value = TRUE)
hear <- hear %>%
  select(id, all_of(cols_to_keep))

# we won't need some of the variables, so let's delete them
hear <- select(hear, -starts_with(c('X1239', 'X1249')))




## Socioeconomic deprivation
deprivation <- read.csv('deprivation.csv')
hear <- merge(hear, deprivation, by = 'id', all.x = TRUE)


## Disorders

# CVD, hypertension, diabetes in their own field IDs
x6150 <- hear %>%
  select(c(id, starts_with('X6150')))
# insert NAs
x6150$missing <- 0
x6150$missing[is.na(x6150$X6150.0.0) | x6150$X6150.0.0 == -3] <- 1
# create separate columns for hypertension and for CVD
x6150$hypertension <- apply(x6150[c('X6150.0.0', 'X6150.0.1', 'X6150.0.2', 'X6150.0.3')], 1, function(row) any(row == 4, na.rm = TRUE))
x6150$cvd <- apply(x6150[c('X6150.0.0', 'X6150.0.1', 'X6150.0.2', 'X6150.0.3')], 1, function(row) any(row %in% c(1,2,3), na.rm = TRUE))


# repeat for other field IDs
x6177 <- hear %>%
  select(c(id, starts_with('X6177')))
x6177$missing <- 0
x6177$missing[is.na(x6177$X6177.0.0) | x6177$X6177.0.0 == -1 | x6177$X6177.0.0 == -3] <- 1
x6177$hypertension <- apply(x6177[c('X6177.0.0', 'X6177.0.1', 'X6177.0.2')], 1, function(row) any(row == 2, na.rm = TRUE))
x6177$diabetes <- apply(x6177[c('X6177.0.0', 'X6177.0.1', 'X6177.0.2')], 1, function(row) any(row == 3, na.rm = TRUE))

x6153 <- hear %>%
  select(c(id, starts_with('X6153')))
x6153$missing <- 0
x6153$missing[is.na(x6153$X6153.0.0) | x6153$X6153.0.0 == -1 | x6153$X6153.0.0 == -3] <- 1
x6153$hypertension <- apply(x6153[c('X6153.0.0', 'X6153.0.1', 'X6153.0.2')], 1, function(row) any(row == 2, na.rm = TRUE))
x6153$diabetes <- apply(x6153[c('X6153.0.0', 'X6153.0.1', 'X6153.0.2')], 1, function(row) any(row == 3, na.rm = TRUE))

x2443 <- hear %>%
  select(c(id, starts_with('X2443'))) %>%
  rename(diabetes = X2443.0.0)
x2443$missing <- 0
x2443$missing[is.na(x2443$X2443.0.0) | x2443$X2443.0.0 == -1 | x2443$X2443.0.0 == -3] <- 1


# self-reported other diagnoses (hypertension: 1065; diabetes: 1220, 1222, 1223)
diagnoses <- hear %>%
  select(c(id, starts_with('X20002')))
diagnoses$hypertension <- apply(diagnoses[c(colnames(select(diagnoses, -id)))], 1, function(row) any(row == 1065, na.rm = TRUE))
diagnoses$diabetes <- apply(diagnoses[c(colnames(select(diagnoses, -id)))], 1, function(row) any(row %in% c(1220, 1222, 1223), na.rm = TRUE))


# combine the above
hear <- hear %>% 
  select(-starts_with(c('X6150', 'X6177', 'X6153', 'X2443', 'X20002')))

hear$hypertension <- 0
hear$hypertension[hear$id %in% filter(x6150, hypertension == TRUE)$id |
                    hear$id %in% filter(x6177, hypertension == TRUE)$id |
                    hear$id %in% filter(x6153, hypertension == TRUE)$id |
                    hear$id %in% filter(diagnoses, hypertension == TRUE)$id] <- 1

hear$cvd <- 0
hear$cvd[hear$id %in% filter(x6150, cvd == TRUE)$id] <- 1

hear$diabetes <- 0
hear$diabetes[hear$id %in% filter(x6177, diabetes == TRUE)$id |
                hear$id %in% filter(x6153, diabetes == TRUE)$id |
                hear$id %in% filter(x2443, diabetes == TRUE)$id |
                hear$id %in% filter(diagnoses, diabetes == TRUE)$id] <- 1

# deal with missing variables
hear$hypertension_missing <- 0
hear$hypertension_missing[hear$id %in% filter(x6150, missing == 1)$id &
                    hear$id %in% filter(x6177, missing == 1)$id &
                    hear$id %in% filter(x6153, missing == 1)$id &
                    hear$id %in% filter(diagnoses, hypertension == FALSE)$id] <- 1
hear$cvd_missing <- 0
hear$cvd_missing[hear$id %in% filter(x6150, missing == TRUE)$id] <- 1

hear$diabetes_missing <- 0
hear$diabetes_missing[hear$id %in% filter(x6177, missing == 1)$id &
                hear$id %in% filter(x6153, missing == 1)$id &
                hear$id %in% filter(x2443, missing == 1)$id &
                hear$id %in% filter(diagnoses, diabetes == FALSE)$id] <- 1


## social isolation, loneliness
hear$X709.0.0[hear$X709.0.0 == -3 | hear$X709.0.0 == -1] <- NA
hear$X709.0.0[hear$X709.0.0 > 1] <- 0
hear$X1031.0.0[hear$X1031.0.0 == -3 | hear$X1031.0.0 == -1] <- NA
hear$X1031.0.0[hear$X1031.0.0 <= 4] <- 0
hear$X1031.0.0[hear$X1031.0.0 > 4] <- 1
hear$X6160.0.0[hear$X6160.0.0 == -3] <- NA
hear$X6160.0.0[hear$X6160.0.0 > 0] <- 0
hear$X6160.0.0[hear$X6160.0.0 == -7] <- 1
hear <- hear %>% mutate(soc_isol = rowSums(across(c(X709.0.0 , X1031.0.0, X6160.0.0)), na.rm = TRUE))
hear$soc_isol[hear$soc_isol >= 2] <- 1
# among those that have no social isolation, check for NAs in any of the three questions
hear <- hear %>% mutate(na_count_0 = rowSums(across(c(X709.0.0, X1031.0.0, X6160.0.0), is.na)))
# those with a score of 0 or 1 could have such a low score because of NAs, so we have to make sure
# if only one is NA and the other two are 0, keep categorised as not isolated
# if two or more are missing, categorise as NA
hear$soc_isol[hear$na_count_0 >= 2] <- NA
hear <- hear %>%
  select(-starts_with(c('X709', 'X1031', 'X6160', 'na_count')))

# education (as in Stevenson et al., cited in Jiang et al.; I don't know how the authors figured out the duration of education)
# 5 higher (college or university degree, other professional qualifications) 1,6
# 4 upper secondary (A levels, AS levels or equivalent) 2
# 3 lower secondary (O levels/GCSEs or equivalent, CSEs or equivalent) 3,4 
# 2 vocational (NVQ or HND or HNC or equivalent) 5
# 1 no secondary education (none of the above) -7
hear$education <- NA
selected_cols <- c("X6138.0.0", "X6138.0.1", "X6138.0.2", "X6138.0.3", "X6138.0.4", "X6138.0.5")
hear <- hear %>%
  mutate(education = replace(education, rowSums(select(., all_of(selected_cols)) == -7, na.rm = TRUE) > 0, 1),
         education = replace(education, rowSums(select(., all_of(selected_cols)) == 5, na.rm = TRUE) > 0, 2),
         education = replace(education, rowSums(sapply(select(., all_of(selected_cols)), function(col) col %in% c(3, 4)), na.rm = TRUE) > 0, 3),
         education = replace(education, rowSums(select(., all_of(selected_cols)) == 2, na.rm = TRUE) > 0, 4),
         education = replace(education, rowSums(sapply(select(., all_of(selected_cols)), function(col) col %in% c(1, 6)), na.rm = TRUE) > 0, 5))

hear <- hear %>%
  select(-starts_with(c('X6138', 'X845')))

# rename variables
hear <- hear %>%
  rename(age = X21022.0.0, sex = X31.0.0, ethnicity = X21000.0.0, income = X738.0.0, smoking = X20116.0.0,
         alcohol = X1558.0.0, met = X22032.0.0, mod_phys = X884.0.0, bmi = X21001.0.0, depressive = X2050.0.0, lonely = X2020.0.0,
         hear_aid = X3393.0.0, hear_loss = X2247.0.0, ass_date = X53.0.0)

# smoking
hear$smoking[hear$smoking == -3] <- NA

# alcohol consumption: 1-3x/month and special occasions in one category
hear$alcohol[hear$alcohol == -3] <- NA
hear$alcohol[hear$alcohol == 5] <- 4

# met minutes: for those without, use number of moderate activity days/week to categorise
# unclear how exactly Jiang et al. categorised; I used https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=540 to make the call
hear$mod_phys[hear$mod_phys == -1 | hear$mod_phys == -3] <- NA
hear$met[is.na(hear$met) & (hear$mod_phys == 0 | hear$mod_phys == 1 | hear$mod_phys == 2)] <- 0
hear$met[is.na(hear$met) & (hear$mod_phys == 3 | hear$mod_phys == 4 | hear$mod_phys == 5 | hear$mod_phys == 6)] <- 1
hear$met[is.na(hear$met) & hear$mod_phys == 7] <- 2
hear$mod_phys <- NULL

# BMI: categorise
hear$bmi_cat <- NA
hear$bmi_cat[hear$bmi < 18.5] <- 1
hear$bmi_cat[hear$bmi >= 18.5 & hear$bmi < 25] <- 2
hear$bmi_cat[hear$bmi >= 25 & hear$bmi < 30] <- 3
hear$bmi_cat[hear$bmi >= 30] <- 4

# depressive symptoms
hear$depressive[hear$depressive == -3 | hear$depressive == -1] <- NA
hear$depressive[hear$depressive == 1 | hear$depressive == 2] <- 1
hear$depressive[hear$depressive == 3] <- 2
hear$depressive[hear$depressive == 4] <- 3

# loneliness
hear$lonely[hear$lonely == -3 | hear$lonely == -1] <- NA

# hearing loss
hear$hear_loss[hear$hear_loss == -3 | hear$hear_loss == -1] <- NA

# hearing aid use
hear$hear_aid[hear$hear_aid == -3 | hear$hear_aid == -1] <- NA

# ethnicity
# missing: -1, -3
# white: 1, 1001, 1002, 1003
# asian/asian-british: 3001, 3002, 3, 3003, 3004, 5
# black/black-british: 4, 4003, 4001, 4002
# other: 2001, 2, 2002, 4002, 2003, 2004, 6 (white and black carribean, mixed, white and black African, white and Asian, other mixed, other)
hear$ethnicity_rec <- hear$ethnicity[]
hear$ethnicity_rec[hear$ethnicity_rec %in% c(-1, -3)] <- NA
hear$ethnicity_rec[hear$ethnicity_rec %in% c(1, 1001, 1002, 1003)] <- 'w'
hear$ethnicity_rec[hear$ethnicity_rec %in% c(3001, 3002, 3, 3003, 3004, 5)] <- 'a'
hear$ethnicity_rec[hear$ethnicity_rec %in% c(4, 4003, 4001, 4002)] <- 'b'
hear$ethnicity_rec[hear$ethnicity_rec %in% c(2001, 2, 2002, 2003, 2004, 6)] <- 'o'


# income harmonisation
hear$income[hear$income == 5] <- 4








## medical diagnoses of dementia

# diagnosis codes
dementia_diagnoses <- data.frame (code  = c('F00', 'F01', 'F02', 'F03', 'G30', 'G310', 'G311', 'G318', '2901'),
                                  source = c(rep('icd10', 8), 'icd9'))
dementia_diagnoses$diagnosis <- NA
dementia_diagnoses$diagnosis[dementia_diagnoses$code %in% c('F00', 'F01', 'G30', '2901')] <- 'AD'
dementia_diagnoses$diagnosis[dementia_diagnoses$code == 'F01'] <- 'VaD'
dementia_diagnoses$diagnosis[dementia_diagnoses$code %in% c('F02', 'F03', 'G310', 'G311', 'G318')] <- 'other'
dementia_diagnoses$n <- 0

# import death data
death <- read.csv('death_causes.csv')
# remove empty rows
death[death == ''] <- NA
death <- death %>%
  filter(rowSums(is.na(select(., -eid))) != ncol(.) - 1)
# keep only rows where any dementia code appears
logical_matrix <- apply(death, 1, function(row) {
  any(grepl(paste(as.vector(dementia_diagnoses$code), collapse="|"), row)) # logical vector for each row; TRUE if at least one column TRUE in each row
})
death <- death[logical_matrix, ]
# remove empty columns
death <- Filter(function(x)!all(is.na(x)), death)
# iterate over each column (except 'id')
for (colname in names(death)) {
  if (colname != 'eid') {
    # values to NA unless they match a code in dementia_diagnoses$code
    mask <- !sapply(death[[colname]], function(cell) {
      any(grepl(paste(dementia_diagnoses$code, collapse="|"), cell))
    })
    death[mask, colname] <- NA
  }
}
# transform to long
death <- death %>%
  pivot_longer(cols = -eid, names_to = "variable", values_to = "code") %>%
  filter(!is.na(code)) %>%
  select(-variable)
death <- as.data.frame(death) %>%
  rename(id = eid) %>%
  distinct(id, .keep_all = TRUE)
# add dates
death_dates <- read.csv('death_date.csv') %>%
  rename(id = eid, date = X40000.0.0) %>%
  filter(date != '')
death_dates$date <- as.Date(death_dates$date, format = '%d/%m/%Y')
death <- merge(death, death_dates, by = 'id', all.x = TRUE)


# repeat similar procedure with hospital data
inpatient <- readRDS('inpatient_diagnoses.rds')
colnames(inpatient)[colnames(inpatient) == 'diagnosis'] <- 'code'
inpatient <- inpatient[grepl(paste(as.vector(dementia_diagnoses$code), collapse="|"), inpatient$code), ]
inpatient$date <- as.Date(inpatient$date, format = '%Y-%m-%d')
# remove duplicate codes and match codes with descriptions
inpatient <- inpatient %>% arrange(date)
inpatient <- distinct(inpatient, id, code, version, .keep_all = TRUE)
inpatient$diagnosis <- NA
inpatient$version <- NULL



# merge death and inpatient
death$diagnosis <- NA
diagnoses <- rbind(inpatient, death)

#search for diagnoses
for(i in 1:nrow(dementia_diagnoses)) {
  current_code <- dementia_diagnoses$code[i]
  current_diagnosis <- dementia_diagnoses$diagnosis[i]

  # rows that match the current code
  matching_rows <- grepl(current_code, diagnoses$code)
  
  # update the diagnosis column for matching rows
  diagnoses$diagnosis[matching_rows] <- current_diagnosis
  
  # update the n column in dementia_diagnoses
  dementia_diagnoses$n[i] <- sum(matching_rows)
}

# remove duplicate diagnoses of the same type and check for multiple types for same participant
diagnoses <- diagnoses %>%
  arrange(date) %>%
  distinct(id, diagnosis, .keep_all = TRUE)


# pivot to wide format and create columns for each diagnosis
diagnoses$code <- NULL
diagnoses <- diagnoses %>%
  pivot_wider(names_from = diagnosis, values_from = date)
diagnoses <- as.data.frame(diagnoses) %>%
  rename(dem_inpatient_ad_date = AD, dem_inpatient_other_date = other, dem_inpatient_vad_date = VaD)

# add to main data frame
hear <- merge(hear, diagnoses, by = 'id', all.x = TRUE)
hear$dem_inpatient_ad <- 0; hear$dem_inpatient_other <- 0; hear$dem_inpatient_vad <- 0
hear$dem_inpatient_ad[!is.na(hear$dem_inpatient_ad_date)] <- 1
hear$dem_inpatient_other[!is.na(hear$dem_inpatient_other_date)] <- 1
hear$dem_inpatient_vad[!is.na(hear$dem_inpatient_vad_date)] <- 1
hear <- hear %>%
  mutate(dem_inpatient_any = if_else(rowSums(select(., dem_inpatient_ad, dem_inpatient_other, dem_inpatient_vad)) >= 1, 1, 0)) %>%
  mutate(dem_inpatient_any_date = reduce(across(c(dem_inpatient_ad_date, dem_inpatient_other_date, dem_inpatient_vad_date)), pmin, na.rm = TRUE))


# add death dates
death_dates <- read.csv('death_date.csv') %>%
  rename(id = eid, death_date = X40000.0.0)
death_dates[death_dates == ''] <- NA
death_dates$death_date <- as.Date(death_dates$death_date, format = '%d/%m/%Y')
hear <- merge(hear, death_dates, by = 'id', all.x = TRUE)
hear$death <- 0
hear$death[!is.na(hear$death_date)] <- 1


# algorithmically-defined outcomes and first occurrences for dementias
hear <- hear %>% 
  mutate(across(c(X130840.0.0, X130842.0.0, X42018.0.0, X42020.0.0, 
                  X42022.0.0, X130836.0.0, X131036.0.0, X130838.0.0), ~as.Date(., format = "%Y-%m-%d")))

hear <- hear %>%
  rename(dem_ado_any_date = X42018.0.0,
         dem_ado_ad_date = X42020.0.0,
         dem_ado_vad_date = X42022.0.0,
         dem_fo_ad_dem_date = X130836.0.0,
         dem_fo_ad_date = X131036.0.0,
         dem_fo_vad_date = X130838.0.0)

hear$dem_ado_any <- 0
hear$dem_ado_any[!is.na(hear$dem_ado_any_date)] <- 1
hear$dem_ado_ad <- 0
hear$dem_ado_ad[!is.na(hear$dem_ado_ad_date)] <- 1
hear$dem_ado_vad <- 0
hear$dem_ado_vad[!is.na(hear$dem_ado_vad_date)] <- 1

# dementia in AD and AD in same category
hear <- hear %>%
  mutate(dem_fo_ad = if_else(rowSums(!is.na(select(., dem_fo_ad_date, dem_fo_ad_dem_date))) > 0, 1, 0)) %>%
  mutate(dem_fo_ad_date = reduce(across(c(dem_fo_ad_date, dem_fo_ad_dem_date)), pmin, na.rm = TRUE))
hear$dem_fo_ad_dem_date <- NULL

# all "other" dementia diagnoses according to first occurrences are a single variable
hear <- hear %>%
  mutate(dem_fo_other = if_else(rowSums(!is.na(select(., X130840.0.0, X130842.0.0))) > 0, 1, 0)) %>%
  mutate(dem_fo_other_date = reduce(across(c(X130840.0.0, X130842.0.0)), pmin, na.rm = TRUE))
hear <- hear %>%
  select(-starts_with('X'))

# any dementia according to first occurrences
hear <- hear %>%
  mutate(dem_fo_any = if_else(rowSums(!is.na(select(., dem_fo_ad_date, dem_fo_vad_date, dem_fo_other_date))) > 0, 1, 0)) %>%
  mutate(dem_fo_any_date = reduce(across(c(dem_fo_ad_date, dem_fo_vad_date, dem_fo_other_date)), pmin, na.rm = TRUE))





### Calculation of follow-up ###

# starting date: date of assessment (ID 53)
hear$ass_date <- as.Date(hear$ass_date, format = '%Y-%m-%d')



## censoring dates: different for different nations; let's infer the nation based on the latest inpatient/GP record

# get latest source of hospital diagnosis
library(data.table)
diagnoses_dates <- read.csv('hesin.txt', sep="\t")
diagnoses_dates$epistart <- as.Date(diagnoses_dates$epistart, format = '%d-%m-%Y')
diagnoses_dates_dt <- as.data.table(diagnoses_dates)
latest_dates <- as.data.frame(diagnoses_dates_dt[, .(epiend = max(epiend, na.rm = TRUE)), by = eid])
latest_dates <- merge(latest_dates, subset(diagnoses_dates, select = c(eid, epiend, dsource)), 
                      by = c('eid', 'epiend'), all.x = TRUE) %>%
  rename(id = eid) %>%
  select(id, dsource) %>%
  distinct(id, .keep_all = TRUE)
# for those with no data, supplement with primary care (1= England(Vision), 2= Scotland, 3 = England (TPP), 4 = Wales)
gp_diagnoses <- read.csv('gp_clinical.txt', sep="\t", header=TRUE, quote="") %>%
  select(eid, data_provider, event_dt) %>%
  mutate(across(event_dt, ~as.Date(., format = "%d/%m/%Y")))
gp_diagnoses_dt <- as.data.table(gp_diagnoses)
gp_latest_dates <- as.data.frame(gp_diagnoses_dt[, .(event_dt = max(event_dt, na.rm = TRUE)), by = eid])
gp_latest_dates <- merge(gp_latest_dates, gp_diagnoses, by = c('eid', 'event_dt'), all.x = TRUE) %>%
  rename(id = eid) %>%
  distinct(id, .keep_all = TRUE)
# use gp-data to supplement by creating a new dsource that includes these imputed values
latest_dates <- merge(latest_dates, gp_latest_dates, by = 'id', all = TRUE)
latest_dates$data_provider[latest_dates$data_provider == 1 | latest_dates$data_provider == 3] <- 'HES'
latest_dates$data_provider[latest_dates$data_provider == 2] <- 'SMR'
latest_dates$data_provider[latest_dates$data_provider == 4] <- 'PEDW'
rm(gp_diagnoses, gp_diagnoses_dt, diagnoses_dates, diagnoses_dates_dt, gp_latest_dates)


## ## Calculate censoring dates
# add latest date of follow-up depending on latest source of diagnosis
# England: 30.9.2021 (Jiang et al.); 31.10.2022 (my data)
# Scotland: 31.7.2021
# Wales: 28.2.2018

# using just inpatient records
latest_dates$d_end_0 = as.Date(c(NA, NA), origin = "1970-01-01")
latest_dates$d_end_0[latest_dates$dsource == 'HES'] <- as.Date('31.10.2022', format = '%d.%m.%Y')
latest_dates$d_end_0[latest_dates$dsource == 'SMR'] <- as.Date('31.07.2021', format = '%d.%m.%Y')
latest_dates$d_end_0[latest_dates$dsource == 'PEDW'] <- as.Date('28.02.2018', format = '%d.%m.%Y')

# use inpatient and GP records
latest_dates$d_end_1 <- latest_dates$d_end_0
latest_dates$d_end_1[is.na(latest_dates$dsource) & latest_dates$data_provider == 'HES'] <- as.Date('31.10.2022', format = '%d.%m.%Y')
latest_dates$d_end_1[is.na(latest_dates$dsource) & latest_dates$data_provider == 'SMR'] <- as.Date('31.07.2021', format = '%d.%m.%Y')
latest_dates$d_end_1[is.na(latest_dates$dsource) & latest_dates$data_provider == 'PEDW'] <- as.Date('28.02.2018', format = '%d.%m.%Y')

latest_dates <- subset(latest_dates, select = -c(dsource, event_dt, data_provider))
hear <- merge(hear, latest_dates, by='id', all.x = TRUE)


# include APOE
apoe <- read.csv('apoe.csv')
colnames(apoe) <- c('id', 'rs429358', 'rs7412')

apoe$apoe <- NA
apoe$apoe[apoe$rs7412 == 0 & apoe$rs429358 == 0] <- 'e1/e1' # TT, CC
apoe$apoe[apoe$rs7412 == 0 & apoe$rs429358 == 1] <- 'e1/e2' # TT, CT
apoe$apoe[apoe$rs7412 == 1 & apoe$rs429358 == 1] <- 'e1/e3 / e2/e4' # CT, CT
apoe$apoe[apoe$rs7412 == 1 & apoe$rs429358 == 0] <- 'e1/e4' # CT, CC
apoe$apoe[apoe$rs7412 == 0 & apoe$rs429358 == 2] <- 'e2/e2' # TT, TT
apoe$apoe[apoe$rs7412 == 1 & apoe$rs429358 == 2] <- 'e2/e3' # CT, TT
apoe$apoe[apoe$rs7412 == 2 & apoe$rs429358 == 2] <- 'e3/e3' # CC, TT
apoe$apoe[apoe$rs7412 == 2 & apoe$rs429358 == 1] <- 'e3/e4' # CC, CT
apoe$apoe[apoe$rs7412 == 2 & apoe$rs429358 == 0] <- 'e4/e4' # CC, CC

# simplified genotypes
apoe$apoe_carrier <- NA
apoe$apoe_carrier[apoe$apoe=='e2/e2' | apoe$apoe=='e2/e3' | apoe$apoe=='e3/e3'] <- 0
apoe$apoe_carrier[apoe$ea7412==0] <- 0
apoe$apoe_carrier[apoe$apoe=='e3/e4' | apoe$apoe=='e2/e4'] <- 1
apoe$apoe_carrier[apoe$apoe=='e4/e4'] <- 2
apoe <- subset(apoe, select=c(id, apoe_carrier))

hear <- merge(hear, apoe, by = 'id', all.x = TRUE)

saveRDS(hear, file = "hear.rds")

