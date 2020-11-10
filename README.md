# 4CE_Comorbidity_Mapping

These scripts adapts the [icd package](https://github.com/jackwasey/icd) by Jack Wasey to facilitate comorbidity mapping of ICD 9 & 10 codes 4CE Phase2.1 PatientObservations.csv file. Specifically, these scripts modify the *icd* functions to map to the truncated ICD codes of the 4CE COVID-19 patients. 

The *icd* functions provide quick mapping to ICD 9 & 10 codes which have been validated as indicators of comorbidities and used in several comobridity scoring indexes including Charlson, Quan-Deyo, and Quan-Elixhauser. 

    |── R

        ├── Comorbidity_Mapping.Rmd <- script to map comorbidities to PatientObservations.csv file
    
        ├── simulated_data.R <- script utilizing the [icd_file_generator](https://github.com/magic-lantern/icd_file_generator) respository, to randomly generate a dataset of patients and ICD codes in the same format as the 4CE's Phase2.1 PatientObservations.csv 
    
    ├── simulated_data        <- simulated dataset derived from R/simulated_data.R

    ├── README.md          <- README for quick introduction to respository

*Note: README directory structure adapted from [Cookiecutter-data-science](https://drivendata.github.io/cookiecutter-data-science/)*
