# Simulate ICD datasets using icdgenerator github (https://github.com/magic-lantern/icd_file_generator)
# The packages would not install on R 3.6.0
# However, they include their raw-data which we can use to randomly generate our own simulated data
# Data simulated is then formatted into the format of the Phase2.1 4CE in order for user's to test the comorbidity mapping script

library(dplyr)

load("icd_file_generator/data/icd10dx.rda")
load("icd_file_generator/data/icd9dx.rda")

set.seed(1234)

# create 100 pseudo-patients for each icd code type
pts10 <- seq(1,100,1)
pts9 <- seq(1,100,1)

pt_dx10_list = list()

# for each pseudo-patient, randomly generate a number (ran)
# this random number will be used to randomly select that many icd codes
for (i in pts10) {

  ran <- sample(1:10, 1)
  dx <- icd10dx[sample(nrow(icd10dx), size = ran, replace = FALSE), ]
  dx$patient_num <- i
  dx$concept_type <- "DIAG-ICD10"
  dx <- dx %>% select(patient_num, concept_type, code)

  pt_dx10_list[[i]] <- dx
}

# unlist and convert to dataframe
pt_dx10 = do.call(rbind, pt_dx10_list)

pt_dx9_list = list()
for (i in pts9) {

  ran <- sample(1:10, 1)
  dx <- icd9dx[sample(nrow(icd9dx), size = ran, replace = FALSE), ]
  dx$patient_num <- i
  dx$concept_type <- "DIAG-ICD9"

  dx <- dx %>% select(patient_num, concept_type, code)

  pt_dx9_list[[i]] <- dx
}

pt_dx9 = do.call(rbind, pt_dx9_list)

# combine both generated icd9 and icd10 datasets
simulated_data <- rbind(pt_dx10, pt_dx9)

# modify code columns; add decimals for our full codes.
# create a separate column to truncate the icd code to its first 3 characters
simulated_data$full_code <- sub("(.{3})(.*)", "\\1.\\2", simulated_data$code)
simulated_data$concept_code <- substr(simulated_data$code, 1, 3)

# for each row, generate random days_since_admission
simulated_data <- simulated_data %>%
  mutate(days_since_admission = sample(-365:100, n(), replace = TRUE)) %>%
  mutate(siteid = "test_site") %>%
  mutate(value = 999) %>%
  select(siteid, patient_num, days_since_admission, concept_type,
         full_code, concept_code, value) %>%
  arrange(patient_num, days_since_admission)

# save simulated data
write.csv(simulated_data, file = "data/simulated_data.csv", row.names = FALSE)
