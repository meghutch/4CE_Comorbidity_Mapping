# 4CE_Comorbidity_Mapping

These scripts adapt the [icd package](https://github.com/jackwasey/icd) by Jack Wasey to facilitate comorbidity mapping of the ICD 9 & 10 codes contained in the 4CE Phase2.1 PatientObservations.csv file. Specifically, these scripts modify the *icd* functions to map to truncated ICD codes.

The *icd* functions provide quick mapping to ICD 9 & 10 codes which have been validated as indicators of comorbidities and used in several comobridity scoring indexes including Charlson, Quan-Deyo, and Quan-Elixhauser. 

## Respository Directory

    |── R

        ├── Comorbidity_Mapping.Rmd <- script to map comorbidities to PatientObservations.csv file
        
        ├── Comorbidity_Mapping_Functions.R <- same script as the Rmd file of the same name, however, this R file can be used by other
        scripts to source in the comorbidity mapping function
        
        ├── Neuro_Comorbidities.Rmd <- analysis examining comorbidities in neurologic patients
    
        ├── simulated_data.R <- script to randomly generate a dataset of patients and ICD codes in the same format as the 4CE's Phase2.1 PatientObservations.csv 
    
    ├── neuro        <- list of icd codes to support the neuro subgroup analysis 
    
    ├── simulated_data        <- simulated dataset derived from R/simulated_data.R

    ├── README.md          <- README for quick introduction to respository

*Note: README directory structure adapted from [Cookiecutter-data-science](https://drivendata.github.io/cookiecutter-data-science/)*
