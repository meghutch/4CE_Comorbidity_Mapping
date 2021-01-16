# 4CE_Comorbidity_Mapping

This repository was constructed to support the mapping of icd codes to comorbidities for projects within the [4CE: Consortium for Clinical Characterization of COVID-19 by EHR ](https://covidclinical.net/). 
Our code adapts the [icd package](https://github.com/jackwasey/icd) by [Jack Wasey](https://github.com/jackwasey) to identify comorbidities from icd codes found in a site's 4CE Phase2.1: LocalPatientObservations.csv.

[*icd*](https://github.com/jackwasey/icd) provides quick mapping to ICD 9 & 10 codes which have been validated as risk factors for mortality within several comorbidity scoring indexes including Charlson, Quan-Deyo, and Quan-Elixhauser. Our scripts modify functions from *icd* in order to support mapping to the 4CE's unique use of truncated ICD codes. Specifically, users can calculate comorbidity scorse using either the Charlson-Deyo or Elixhauser-van-walvaren mappings. Additionally, users of our ```'map_char_elix_codes'``` function can specify the timeline of mapping in relation to a patient's index admission. 

Functions found in this repo have been developed by [Trang Le](https://github.com/trang1618) and [Meghan Hutch](https://github.com/meghutch) and incoporated in the the 4CE's [COVID-19 & Neurological Disease Analysis](https://github.com/trang1618/neuro-penn). 

Please see the Comorbidity_Mapping_Demo.Rmd to learn how to use the```'map_char_elix_codes'`` function.

## Respository Directory

    ├── Comorbidity_Mapping_Demo.Rmd - Script that shows use of comorbitity mapping fucntions
    
    ├── R

        ├── comorbidity-map-funcs.R
    
        ├── simulate_data.R <- script to randomly generate a dataset of patients and ICD codes in the same format as the 4CE's Phase2.1 PatientObservations.csv 
    
    ├── data        
    
        ├── simulated_data <- simulated dataset derived from R/simulated_data.R

    ├── README.md <- README for quick introduction to respository

*Note: README directory structure adapted from [Cookiecutter-data-science](https://drivendata.github.io/cookiecutter-data-science/)*
