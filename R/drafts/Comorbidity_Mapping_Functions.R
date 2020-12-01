library(dplyr)
library(icd)
library(kableExtra)
library(purrr)
library(tableone)

# The below was copied from the Comorbidity_Mapping.Rmd file in order to support sourcing it for other analyses.

map_charlson_codes <- function(data) { 
  
  data <- data %>% filter(concept_type == "DIAG-ICD10" | concept_type == "DIAG-ICD9")
  
  # filter for diagnoses prior to admission
  data <- data %>% filter(days_since_admission < 0)
  
  # Create separate data frames for ICD9 and 10 Codes
  # icd package does not support simultaneous processing of both ICD code types
  # we will recombine after the initial processing
  icd10 <- data %>% 
    filter(concept_type == "DIAG-ICD10") %>% 
    distinct()
  
  icd9 <- data %>%
    filter(concept_type == "DIAG-ICD9") %>% 
    distinct()
  
  ## Because the 4CE has truncated ICD codes, we will also truncate the icd package index maps
  # Function to select first 3 characters of the ICD Code in all lists of the index map
  first_3 <- function(x) { 
    x <- substr(x, 1, 3)
    return(x)
  }
  
  icd10_map_charlson3 <- lapply(icd10_map_charlson, sapply, first_3)
  icd9_map_charlson3 <- lapply(icd9_map_charlson, sapply, first_3)
  
  # perform the mapping
  icd10_map <- icd10_comorbid(icd10, map = icd10_map_charlson3, icd_name = "concept_code",
                              return_df = TRUE, visit_name = "patient_num", 
                              return_binary = TRUE)
  
  icd9_map <- icd9_comorbid(icd9, map = icd9_map_charlson3, icd_name = "concept_code", 
                            return_df = TRUE, visit_name = "patient_num", 
                            return_binary = TRUE)
  
  
  # Combine the results of the 9 and 10 mapping
  icd_map <- rbind(icd9_map, icd10_map)
  
  # If multiple rows due to a patient having both ICD 9 and 10 codes, we will take the max of the column 
  # This will allow us to capture the 1s indicating that the comorbidity is present
  # the try wrapper is important in cases there is not an instance of a specific comorbidity in the data - try silences errors
  icd_map <- icd_map %>% 
    group_by(patient_num) %>% 
    mutate(MI = try(max(MI), silent = TRUE)) %>%
    mutate(CHF = try(max(CHF), silent = TRUE)) %>%
    mutate(PVD = try(max(PVD), silent = TRUE)) %>%
    mutate(Stroke = try(max(Stroke), silent = TRUE)) %>% 
    mutate(Dementia = try(max(Dementia), silent = TRUE)) %>%
    mutate(Pulmonary = try(max(Pulmonary), silent = TRUE)) %>%
    mutate(Rheumatic = try(max(Rheumatic), silent = TRUE)) %>%
    mutate(PUD = try(max(PUD), silent = TRUE)) %>%
    mutate(LiverMild = try(max(LiverMild), silent = TRUE)) %>%
    mutate(DM = try(max(DM), silent = TRUE)) %>%
    mutate(DMcx = try(max(DMcx), silent = TRUE)) %>%
    mutate(Paralysis = try(max(Paralysis), silent = TRUE)) %>%
    mutate(Renal = try(max(Renal), silent = TRUE)) %>%
    mutate(Cancer = try(max(Cancer), silent = TRUE)) %>%
    mutate(LiverSevere = try(max(LiverSevere), silent = TRUE)) %>%
    mutate(Mets = try(max(Mets), silent = TRUE)) %>%
    mutate(HIV = try(max(HIV), silent = TRUE)) %>%
    slice(1L)
  
  icd_map <- icd_map %>% arrange(as.numeric(patient_num))
  
  table1 <- as.data.frame(colSums(Filter(is.numeric,icd_map)))
  table1 <- tibble::rownames_to_column(table1, "Comorbidity")
  
  # replace abbreviations with full comorbidity name
  names_charlson_df = map_df(names_charlson, ~as.data.frame(.x), 
                             .id="code")
  colnames(names_charlson_df)[2] <- "Name"
  
  names_charlson_abbrev_df <- map_df(names_charlson_abbrev, ~as.data.frame(.x),
                                     .id="code")
  colnames(names_charlson_abbrev_df)[2] <- "Comorbidity"
  
  comorb_list_names <- full_join(names_charlson_df, names_charlson_abbrev_df,
                                 by = "code")
  
  table1 <- merge(table1, comorb_list_names, by = "Comorbidity")
  table1$code <- NULL
  colnames(table1)[2] <- "Patients"
  table1 <- table1 %>%
    select(-Comorbidity) %>%
    arrange(desc(Patients)) %>%
    mutate(percent = round(Patients/nrow(icd_map)*100,2)) %>%
    select(Name, Patients, percent)
  colnames(table1)[1] <- "Comorbidity"
  
  ## Calculate Index Scores
  charlson_score = charlson_from_comorbid(icd_map, 
                                          visit_name = "patient_num", 
                                          scoring_system = "charlson",  
                                          hierarchy = TRUE)
  
  quan_score = charlson_from_comorbid(icd_map, 
                                      visit_name = "patient_num", 
                                      scoring_system = "quan", 
                                      hierarchy = TRUE)
  
  charlson_score <- as.data.frame(charlson_score)
  charlson_score <- tibble::rownames_to_column(charlson_score, "patient_num")
  
  quan_score <- as.data.frame(quan_score)
  quan_score <- tibble::rownames_to_column(quan_score, "patient_num")
  
  index_scores <- full_join(icd_map, charlson_score, by = "patient_num")
  index_scores <- full_join(index_scores, quan_score, by = "patient_num")
  
  # calculate difference between Charlson and Quan scores
  index_scores <- index_scores %>%
    arrange(as.numeric(patient_num)) %>%
    mutate(score_diff = charlson_score - quan_score) %>% 
    mutate(abs_score_diff = abs(score_diff)) %>%
    arrange(desc(charlson_score)) %>%
    select(patient_num, charlson_score, quan_score,
           score_diff, abs_score_diff, everything())
  
  
  
  ## Identify the specific codes that mapped
  
  # First we will add the comorbidity abbreviation adjacent to the mapped ICD Code
  comorb_names = c("MI", "CHF", "PVD", "Stroke", "Dementia", "Pulmonary", "Rheumatic",
                   "PUD", "LiverMild", "DM", "DMcx", "Paralysis", "Renal", "Cancer",
                   "LiverSevere", "Mets", "HIV")
  
  # unlist the charlson mapping lists
  icd10_code_map <- map_df(icd10_map_charlson3, ~as.data.frame(.x), .id="name")
  colnames(icd10_code_map) <- c("Comorbidity", "concept_code")
  
  icd10_code_map$concept_code <- as.character(icd10_code_map$concept_code)
  
  icd10_code_map <- icd10_code_map %>% 
    filter(!concept_code %in% comorb_names) %>%
    distinct()
  
  icd9_code_map <- map_df(icd9_map_charlson3, ~as.data.frame(.x), .id="name")
  colnames(icd9_code_map) <- c("Comorbidity", "concept_code")
  
  icd9_code_map <- icd9_code_map %>% 
    filter(!concept_code %in% comorb_names) %>%
    distinct()
  
  # merge the mapping dataframe to the patient level ICD codes
  # this will return all comorbidities that mapped to our patient data
  icd10_map <- inner_join(icd10, icd10_code_map, by = "concept_code")
  icd9_map <- inner_join(icd9, icd9_code_map, by = "concept_code")
  
  # explain_codes will add additional information regarding the code name 
  # add if statements in order to handle sites that only have ICD 9 or 10 codes but not both
  
  if (nrow(icd10_map) > 0) {
    
    icd10_mapped_table <- explain_table(icd10_map$concept_code)
    colnames(icd10_mapped_table)[1] <- "concept_code"
    icd10_mapped_table <- icd10_mapped_table %>% distinct()
    
    icd10_mapped_table <- left_join(icd10_map, icd10_mapped_table, by = "concept_code")
    
    # keep columns of interest 
    icd10_mapped_table <- icd10_mapped_table %>%
      select(patient_num, concept_code, Comorbidity, long_desc) %>% 
      distinct()
  }
  
  if (nrow(icd9_map) > 0) {
    
    icd9_mapped_table <- explain_table(icd9_map$concept_code)
    colnames(icd9_mapped_table)[1] <- "concept_code"
    icd9_mapped_table <- icd9_mapped_table %>% distinct()
    
    icd9_mapped_table <- left_join(icd9_map, icd9_mapped_table, by = "concept_code")
    
    # keep columns of interest 
    icd9_mapped_table <- icd9_mapped_table %>%
      select(patient_num, concept_code, Comorbidity, long_desc) %>% 
      distinct()
  } 
  
  
  # Bind both ICD 9 and 10 code tables togethe
  mapped_codes_table <- rbind(try(icd10_mapped_table, icd9_mapped_table), silent = TRUE)
  
  # calculate how many patients had each unique Comorbidity/concept_code
  mapped_codes_table <- mapped_codes_table %>%
    group_by(concept_code, Comorbidity, long_desc) %>%
    mutate(n_patients = n()) %>%
    ungroup() %>%
    group_by(long_desc, n_patients) %>%
    arrange(desc(n_patients)) %>%
    select(-patient_num) %>%
    distinct()
  
  map_results <- list(comorb_list_names, icd_map, index_scores, table1, mapped_codes_table)
  
  return(map_results)
  
}