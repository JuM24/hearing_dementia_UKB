# This code combines data fields 41270, 41271, 41280, and 41281, which refer to inpatient diagnoses and dates of admission.
# For 6 participants, array indexing when combining diagnoses codes and dates fails, so I used the record-level data to access
# hospital addmissions for those 6 participants.

# The final output of this code is a data frame in long format with id, diagnosis, date of admission, and icd version.


library(tidyverse)
library(zoo)


# inpatient
diagnoses <- read.csv('inpatient.csv')
#colnames(diagnoses) <- c('id', 'icd10', 'icd10_date', 'icd9', 'icd9_date')

diagnoses[diagnoses==""]  <- NA 
diagnoses <- as.data.frame(sapply(diagnoses, as.character))
colnames(diagnoses)[colnames(diagnoses) == 'eid'] <- 'id'

# separate sources of diagnoses (ICD9 vs 10) and dates vs. diagnosis codes
icd9 <- diagnoses %>% select(c('id', starts_with('X41271')))
icd9_date <- diagnoses %>% select(c('id', starts_with('X41281')))
icd10 <- diagnoses %>% select(c('id', starts_with('X41270')))
icd10_date <- diagnoses %>% select(c('id', starts_with('X41280')))

# keep only rows without NAs
icd9 <- icd9[rowSums(is.na(icd9))!=ncol(icd9)-1,]
icd9_date <- icd9_date[rowSums(is.na(icd9_date))!=ncol(icd9_date)-1,]
icd10 <- icd10[rowSums(is.na(icd10))!=ncol(icd10)-1,]
icd10_date <- icd10_date[rowSums(is.na(icd10_date))!=ncol(icd10_date)-1,]

# transform to long-type format
icd9_long <- icd9 %>%  pivot_longer(-id, names_to = "diagnosis", values_drop_na=TRUE); colnames(icd9_long) <- c('id', 'column', 'diagnosis'); icd9_long$column <- sub('X41271.', '', icd9_long$column)
icd9_date_long <- icd9_date %>%  pivot_longer(-id, names_to = "diagnosis", values_drop_na=TRUE); colnames(icd9_date_long) <- c('id', 'column', 'date'); icd9_date_long$column <- sub('X41281.', '', icd9_date_long$column)
icd10_long <- icd10 %>%  pivot_longer(-id, names_to = "diagnosis", values_drop_na=TRUE); colnames(icd10_long) <- c('id', 'column', 'diagnosis');  icd10_long$column <- sub('X41270.', '', icd10_long$column)
icd10_date_long <- icd10_date %>%  pivot_longer(-id, names_to = "diagnosis", values_drop_na=TRUE); colnames(icd10_date_long) <- c('id', 'column', 'date');  icd10_date_long$column <- sub('X41280.', '', icd10_date_long$column)

# combine all diagnoses
icd9 <- merge(icd9_long, icd9_date_long, by = c('id', 'column')); icd9$column <- NULL; icd9$version <- 'icd9'
icd10 <- merge(icd10_long, icd10_date_long, by = c('id', 'column')); icd10$column <- NULL; icd10$version <- 'icd10'
inpatient <- rbind(icd9, icd10)

# some participants are missing from the icd10 date file; identify those
ids_missing <- unique(filter(merge(icd10_long, icd10_date_long, by = c('id', 'column'), all = TRUE), is.na(date)) %>% select(id))

# get the missing participants from the master hesin table
diagnoses_dates <- read.csv('hesin.txt', sep="\t")
diagnoses <- read.csv('hesin_diag.txt', sep="\t")
diagnoses_dates <- filter(diagnoses_dates, eid %in% ids_missing$id)
diagnoses <- filter(diagnoses, eid %in% ids_missing$id)
diagnoses_dates <- subset(diagnoses_dates, select=c(eid, ins_index, admidate))
diagnoses <- subset(diagnoses, select=c(eid, ins_index, diag_icd9, diag_icd10))
diagnoses$eid <- as.character(diagnoses$eid)
diagnoses_dates$eid <- as.character(diagnoses_dates$eid)
diagnoses <- merge(diagnoses, diagnoses_dates, by=c('eid', 'ins_index'), all.x = TRUE)
diagnoses <- subset(diagnoses, select = c(eid, diag_icd10, admidate))
colnames(diagnoses) <- c('id', 'diagnosis', 'date'); diagnoses$version <- 'icd10'

inpatient <- rbind(inpatient, diagnoses)

saveRDS(inpatient, file = "inpatient_diagnoses.rds")
write.csv(inpatient, 'inpatient_diagnoses.csv', row.names = FALSE)
