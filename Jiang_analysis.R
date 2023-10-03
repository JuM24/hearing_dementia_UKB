# This code mainly consists of one function to clean the data frame and print out the effect (and CIs) for the
# exposure (hearing loss) for both groups (HA-users + non-HL; HA-non-users + non-HL). The arguments to the function
# specify the sensitivity analysis; the default arguments keep the analysis as per default.
# In the end, there is also some code to generate basic descriptive stats.



library(tidyverse)
library(gsubfn)

hear <- readRDS('D:/Job/Projects/Luxembourg/hearing aids/Jiang-commentary/hear.Rds')


## The `analyse_hearing` function takes in a number of arguments that refer to different ways in which
# data-preparation could be done. It can be used to run the default analysis and sensitivity analyses

# Here are the options for each argument. The first argument in each array is the function default and the analysis default.
# The numbers on the right correspond directly to numbers of sensitivity analyses in the manuscript. The final three
# arguments are tehnically not sensitivity analyses but just variations of both the default and the sensitivity analyses
# and correspond to variations in the outcome of interest (`dem_subtype`), modelling approach used ('model'),
# and adjustment (basic vs. full; `adjustment`).

# follow_up <- c('use_both', 'use_inpatient') # 1
# censoring_na <- c('wales', 'remove', 'england') # 2a, 2b
# outcome_dates <- c('shift', 'remove', 'recode') # 3a, 3b
# dem_asc <- c('ado', 'inpatient', 'fo') # 4a, 4b; 'ado' is default
# missing <- c('remove', 'keep') # 5
# dem_subtype <- c('any', 'ad', 'vad')
# model <- c('HR', 'RR')
# adjustment <- c('1', '2', '3', '4')

# The function returns the dataset, cleaned as per arguments given, and both models that were run
# (i.e., HA users vs. no HL; HA non-users vs. no HL). It also prints out numbers of transformed/removed
# observations at various points and the main effects and 95% CIs for each of the two models.

# Here is an example usage:

# list[data_clean, ha_model, no_model] <- analyse_hearing(hear, model = 'HR', dem_subtype = 'vad', adjustment = '4',
#                                                        follow_up = 'use_inpatient')

# The above function would calculate a fully adjusted (`adjustment=4`) Cox proportional hazards model (`model='HR'`) 
# for the outcome vascular dementia (`dem_subtype='vad'`). It would calculate follow-up using censor-date allocation 
# from inpatient records only (`follow_up='use_inpatient'`). All other arguments would correspond to the default
# analysis as described in the manuscript and as defined in the function definition below.

# The function also has an implementation for "intermediate" adjustment variations (as per Jiang et al., 2023),
# and for competing risks. However, these were not used in the final analyses and were also not error-tested.

analyse_hearing <- function(data, follow_up='use_both', censoring_na='wales', outcome_dates='shift',
                            dem_asc = 'ado', missing='remove', dem_subtype = 'any', model = 'RR', adjustment = '1'){
  
  data_clean <- data.frame(data)
  
  # deafness as hearing loss
  data_clean$hear_loss[data_clean$hear_loss == 99] <- 1
  
  # no hearing assessment
  data_clean <- filter(data_clean, !is.na(hear_loss))
  
  # hearing aid to 0 when hearing aid is missing
  data_clean$hear_aid[is.na(data_clean$hear_aid)] <- 0
  
  # determine column to use for outcome
  columns <- colnames(data_clean %>% 
                        select(ends_with(paste0('_', dem_subtype))))
  # find indices of matching strings and subset
  matching_indices <- grep(paste0("_", dem_asc, "_"), columns, value = FALSE)
  columns <- columns[matching_indices]
  dem_outcome <- columns[1]
  dem_outcome_date <- paste0(dem_outcome, '_date')
  # set the choice as the outcome
  data_clean$dementia <- data_clean[[dem_outcome]]
  data_clean$dementia_date <- data_clean[[dem_outcome_date]]
  print(paste0('Predicting >', dem_outcome, '< diagnosed on >', dem_outcome_date, '<.'))
  
  # dementia at baseline (remove any dementia at or before baseline)
  columns <- colnames(data_clean %>% 
                        select(ends_with('_any')))
  matching_indices <- grep(paste0("_", dem_asc, "_"), columns, value = FALSE)
  columns <- columns[matching_indices]
  dem_outcome <- columns[1]
  dem_outcome_date <- paste0(dem_outcome, '_date')
  data_clean$dementia_any <- data_clean[[dem_outcome]]
  data_clean$dementia_any_date <- data_clean[[dem_outcome_date]]
  data_clean <- filter(data_clean, dementia_any_date > ass_date | dementia_any == 0)
  
  # remove those with hearing aids without hearing loss
  to_remove <- filter(data_clean, hear_aid == 1 & hear_loss == 0)$id
  data_clean <- filter(data_clean, !id %in% to_remove)
  
  # missing confounder data
  data_clean$income[data_clean$income %in% c(-1, -3)] <- NA
  data_clean$hypertension[data_clean$hypertension_missing == 1] <- NA
  data_clean$diabetes[data_clean$diabetes_missing == 1] <- NA
  data_clean$cvd[data_clean$cvd_missing == 1] <- NA
  
  # which source to use when computing follow-up
  if (follow_up == 'use_inpatient'){
    data_clean$d_end <- data_clean$d_end_0
    print(paste0('Imputing censor data using inpatient; ', as.character(sum(!is.na(data_clean$d_end))), ' observations available.'))
  } else if (follow_up == 'use_both') {
    data_clean$d_end <- data_clean$d_end_1
    print(paste0('Imputing censor data using inpatient and GP; ', as.character(sum(!is.na(data_clean$d_end))), ' observations available.'))
  } else {stop('Error: incorrect argument specification.')}
  
  # set censoring date for those missing both GP and inpatient data
  data_clean$censor_date <- data_clean$d_end
  censor_nas <- length(data_clean$censor_date[is.na(data_clean$censor_date)])
  if(censoring_na == 'england'){
    data_clean$censor_date[is.na(data_clean$censor_date)] <- as.Date("31.10.2022", format = '%d.%m.%Y')
    print(paste0('Setting ', as.character(censor_nas), ' to English dates.'))
  } else if (censoring_na == 'remove') {
    data_clean <- filter(data_clean, !is.na(censor_date))
    print(paste0('Removing ', as.character(censor_nas), ' observations without censor dates.'))
  } else if (censoring_na == 'wales') {
    data_clean$censor_date[is.na(data_clean$censor_date)] <- as.Date("28.02.2018", format = '%d.%m.%Y')
    print(paste0('Setting ', as.character(censor_nas), ' to Welsh dates.'))
  } else {stop('Error: incorrect argument specification.')}
  
  # how to deal with outcome after right-censoring
  dem_after <- as.character(length(data_clean$censor_date[data_clean$dementia == 1 & data_clean$dementia_date > data_clean$censor_date]))
  death_after <- as.character(length(data_clean$censor_date[data_clean$death == 1 & data_clean$dementia == 0 & data_clean$death_date > data_clean$censor_date]))
  if (outcome_dates == 'remove'){
    data_clean$dementia[data_clean$dementia == 1 & data_clean$dementia_date > data_clean$censor_date] <- NA
    data_clean$censor_date[data_clean$dementia == 1 & data_clean$dementia_date > data_clean$censor_date] <- NA
    data_clean$death[data_clean$death == 1 & data_clean$dementia == 0 & data_clean$death_date > data_clean$censor_date] <- NA
    data_clean$censor_date[data_clean$death == 1 & data_clean$dementia == 0 & data_clean$death_date > data_clean$censor_date] <- NA
    print(paste0(dem_after, ', ', death_after, ' observations of dementia and death ', outcome_dates, 'd.'))
  } else if (outcome_dates == 'recode'){
    data_clean$dementia[data_clean$dementia == 1 & data_clean$dementia_date > data_clean$censor_date] <- 0 # diagnosis to 0
    data_clean$dementia_date[data_clean$dementia == 1 & data_clean$dementia_date > data_clean$censor_date] <- NA # date of diagnosis to NA
    data_clean$death[data_clean$death == 1 & data_clean$dementia == 0 & data_clean$death_date > data_clean$censor_date] <- 0 # death to 0
    data_clean$death_date[data_clean$death == 1 & data_clean$dementia == 0 & data_clean$death_date > data_clean$censor_date] <- NA # death date to 0 
    print(paste0(dem_after, ', ', death_after, ' observations of dementia and death ', outcome_dates, 'd.'))
  } else if (outcome_dates == 'shift') {
    # if any dementia occurs after censoring, set that as censoring
    data_clean$censor_date[data_clean$dementia == 1 & data_clean$dementia_date > data_clean$censor_date] <- 
      data_clean$dementia_date[!is.na(data_clean$dementia_date) & data_clean$dementia_date > data_clean$censor_date]
    # if death occurs after censoring set death as censoring in those without dementia
    data_clean$censor_date[data_clean$death == 1 & data_clean$dementia == 0 & data_clean$death_date > data_clean$censor_date] <- 
      data_clean$death_date[data_clean$death == 1 & data_clean$dementia == 0 & data_clean$death_date > data_clean$censor_date]
  } else {stop('Error: incorrect argument specification.')}
  
  # determine follow_up
  data_clean$follow_up <- as.numeric(difftime(data_clean$censor_date, data_clean$ass_date, units='days'))/365.25

  
  # whether to remove missing observations or not
  prev_len <- nrow(data_clean)
  if (missing == 'keep'){
  } else if (missing == 'remove') {
    data_clean <- data_clean %>%
      filter(if_all(c(sex, ethnicity_rec, education, income, deprivation, smoking, alcohol, met, bmi_cat,
                      hypertension, diabetes, cvd, apoe_carrier), ~ !is.na(.)))
    print(paste0('Removed ', as.character(prev_len-nrow(data_clean)), ' observations with missing confounders.'))
  } else {stop('Error: incorrect argument specification.')}
  

  ## Modelling
  
  if (adjustment == '1'){
    covariates <- c('age')
  } else if (adjustment == '2'){
    covariates <- c('age', 'sex', 'ethnicity_rec', 'education', 'income', 'deprivation')
  } else if (adjustment == '3'){
    covariates <- c('age', 'sex', 'ethnicity_rec', 'education', 'income', 'deprivation',
                    'smoking', 'alcohol', 'met', 'bmi')
  } else if (adjustment == '4'){
    covariates <- c('age', 'sex', 'ethnicity_rec', 'education', 'income', 'deprivation',
                    'smoking', 'alcohol', 'met', 'bmi_cat', 'hypertension', 'diabetes', 'cvd', 'apoe_carrier')
  }

  
  # censoring event (for competing risk model)
  data_clean$event <- 0
  data_clean$event[data_clean$death == 1] <- 2
  data_clean$event[data_clean$dementia == 1] <- 1 # if dementia diagnosis before death
  
  # subsample of those without hearing loss or with hearing loss and with hearing aids
  hear_aids <- filter(data_clean, (hear_loss == 1 & hear_aid == 1) | hear_loss == 0)
  
  # subsample of those without hearing loss or with hearing loss and without hearing aids
  no_hear_aids <- filter(data_clean, (hear_loss == 1 & hear_aid == 0) | hear_loss == 0)
  
  if (model == 'RR'){
    data_clean_copy <- data_clean[, c('id', 'dementia', 'hear_loss', 'hear_aid', covariates)]
    data_clean_copy <- data_clean_copy[complete.cases(data_clean_copy), ]
    
    formula <- as.formula(paste0('dementia ~ hear_loss',  '+', paste(covariates, collapse = '+')))
    print('Running GLM...')
    fit_yes <- glm(formula, data = hear_aids, family = binomial())
    fit_no <- glm(formula, data = no_hear_aids, family = binomial())
    
    results_yes <- marginaleffects::avg_comparisons(fit_yes,
                                                    variables = c("hear_loss"),
                                                    vcov = TRUE,
                                                    comparison = "lnratioavg",
                                                    transform = "exp")
    
    results_no <- marginaleffects::avg_comparisons(fit_no,
                                                   variables = c("hear_loss"),
                                                   vcov = TRUE,
                                                   comparison = "lnratioavg",
                                                   transform = "exp")
    
    print(paste0('Total n of sample: ', as.character(nrow(data_clean_copy)), '.'))
    print(paste0('HA: ', 'effect=', as.character(round(results_yes$estimate, 2)), ', CI: ', 
                 as.character(round(results_yes$conf.low, 2)), '-',
                 as.character(round(results_yes$conf.high, 2))))
    
    print(paste0('No HA: ', 'RR=', as.character(round(results_no$estimate, 2)), ', CI: ', 
                 as.character(round(results_no$conf.low, 2)), '-',
                 as.character(round(results_no$conf.high, 2))))
  } else if (model == 'HR'){
    print('Running Cox model...')
    data_clean_copy <- data_clean[, c('id', 'dementia', 'hear_loss', 'hear_aid', 'follow_up', covariates)]
    data_clean_copy <- data_clean_copy[complete.cases(data_clean_copy), ]
    formula <- as.formula(paste0('survival::Surv(follow_up, dementia) ~ hear_loss',  '+', paste(covariates, collapse = '+')))
    
    fit_yes <- summary(survival::coxph(formula, data = hear_aids))$coefficients
    fit_no <- summary(survival::coxph(formula, data = no_hear_aids))$coefficients
    
    print(paste0('Total n of sample: ', as.character(nrow(data_clean_copy)), '.'))
    print(paste0('HA: ', 'HR=', as.character(round(fit_yes['hear_loss', 2], 2)), ' CI: ',
                 as.character(round(exp(fit_yes['hear_loss', 1] - 1.96*fit_yes['hear_loss', 3]), 2)), '-',
                 as.character(round(exp(fit_yes['hear_loss', 1] + 1.96*fit_yes['hear_loss', 3]), 2))))
    print(paste0('No HA: ', 'HR=', as.character(round(fit_no['hear_loss', 2], 2)), ' CI: ',
                 as.character(round(exp(fit_no['hear_loss', 1] - 1.96*fit_no['hear_loss', 3]), 2)), '-',
                 as.character(round(exp(fit_no['hear_loss', 1] + 1.96*fit_no['hear_loss', 3]), 2))))
    
  } else if (model == 'competing'){
    print('Running competing risk model...')
    data_clean_copy <- data_clean[, c('id', 'dementia', 'hear_loss', 'hear_aid', 'follow_up', 'death', paste(covariates, collapse = '+'))]
    data_clean_copy <- data_clean_copy[complete.cases(data_clean_copy), ]

    cif_aa_yes <- cmprsk::cuminc(ftime=hear_aids$follow_up, fstatus=hear_aids$event, group=hear_aids$hear_loss, cencode=0)
    cif_aa_no <- cmprsk::cuminc(ftime=no_hear_aids$follow_up, fstatus=no_hear_aids$event, group=no_hear_aids$hear_loss, cencode=0)
    
    # plot KM curves
    plot_yes <- plot(cif_aa_yes, xlab='years', ylab='Probability',col = c(1,2,3,4), main='HA users')
    plot_no <- plot(cif_aa_no, xlab='years', ylab='Probability',col = c(1,2,3,4), main='HA non-users')
    gridExtra::grid.arrange(plot_yes, plot_no, nrow=2)
    
    fit_yes <- cmprsk::crr(ftime=hear_aids$follow_up, fstatus=hear_aids$event, cov1=hear_aids[, covariates], failcode = 1)
    fit_no <- cmprsk::crr(ftime=no_hear_aids$follow_up, fstatus=no_hear_aids$event, cov1=no_hear_aids[, covariates], failcode = 1)
  }
  return(list(data_clean, hear_aids, no_hear_aids))
}






## Some descriptives



# numbers of missing observations
sum(is.na(data_clean$sex))
sum(is.na(data_clean$ethnicity_rec))
sum(is.na(data_clean$education))
sum(is.na(data_clean$income))
sum(is.na(data_clean$deprivation))
sum(is.na(data_clean$smoking))
sum(is.na(data_clean$alcohol))
sum(is.na(data_clean$met))
sum(is.na(data_clean$bmi_cat))
sum(is.na(data_clean$hypertension))
sum(is.na(data_clean$diabetes))
sum(is.na(data_clean$cvd))
sum(is.na(data_clean$follow_up))
sum(is.na(data_clean$apoe_carrier))

# descriptives
table(data_clean$hear_loss); round(100*prop.table(table(data_clean$hear_loss)), 2)
nrow(filter(data_clean, hear_loss == 1 & hear_aid == 1))
nrow(filter(data_clean, hear_loss == 1 & hear_aid == 0))

data_clean$age_cat <- NA
data_clean$age_cat[data_clean$age < 50] <- 1
data_clean$age_cat[data_clean$age >= 50 & data_clean$age <= 59] <- 2
data_clean$age_cat[data_clean$age >= 60 & data_clean$age <= 69] <- 3
data_clean$age_cat[data_clean$age > 69] <- 4
table(data_clean$age_cat); round(100*prop.table(table(data_clean$age_cat)), 2)
table(data_clean$sex); round(100*prop.table(table(data_clean$sex)), 2)

table(data_clean$ethnicity_rec); round(100*prop.table(table(data_clean$ethnicity_rec)), 2)
table(data_clean$income); round(100*prop.table(table(data_clean$income)), 2)

deprivation <- read.csv('D:/Job/Raw data/deprivation.csv')
deprivation <- filter(deprivation, !is.na(deprivation))
depr_quants <- as.list(quantile(deprivation$deprivation, prob=c(.25,.5,.75)))
data_clean$deprivation_cat <- NA
data_clean$deprivation_cat[data_clean$deprivation < depr_quants[1]] <- 1
data_clean$deprivation_cat[data_clean$deprivation >= depr_quants[1] & data_clean$deprivation <= depr_quants[2]] <- 2
data_clean$deprivation_cat[data_clean$deprivation >= depr_quants[2] & data_clean$deprivation <= depr_quants[3]] <- 3
data_clean$deprivation_cat[data_clean$deprivation > depr_quants[3]] <- 4
table(data_clean$deprivation_cat); round(100*prop.table(table(data_clean$deprivation_cat)), 2)

table(data_clean$bmi_cat); round(100*prop.table(table(data_clean$bmi_cat)), 2)
table(data_clean$smoking); round(100*prop.table(table(data_clean$smoking)), 2)
table(data_clean$alcohol); round(100*prop.table(table(data_clean$alcohol)), 2)
table(data_clean$diabetes); round(100*prop.table(table(data_clean$diabetes)), 2)
table(data_clean$hypertension); round(100*prop.table(table(data_clean$hypertension)), 2)
table(data_clean$cvd); round(100*prop.table(table(data_clean$cvd)), 2)
table(data_clean$apoe_carrier); round(100*prop.table(table(data_clean$apoe_carrier)), 2)

table(hear_aids$hear_loss, hear_aids$dementia); round(100*prop.table(table(hear_aids$hear_loss, hear_aids$dementia)), 2)
table(no_hear_aids$hear_loss, no_hear_aids$dementia); round(100*prop.table(table(no_hear_aids$hear_loss, no_hear_aids$dementia)), 2)
