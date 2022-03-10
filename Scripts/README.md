Scripts Documentation
================

## Codes Structure

    .
    ├── 00_downloader_sinan_dengue.R         # Doownloader function based on MicroDataSUS function
    ├── 01_dbf_organizer.R                   # Code on steps to organize and clean dbc and dbf files
    ├── 01b_all_together_organizer.R         # Code on organizing all data in one file
    ├── 02_plots_descriptive.R               # Code for descriptive plots
    ├── 03_aggregating_climate_vars_cases.R  # Code for aggregating climate variables with the cases time series
    ├── pega_pop_datasus_fx_regiao.R         # Function to take dataSUS estimates on population
    │
    └── README.md

## Observations

The `01_dbf_organizer.R` code reads .dbc and .dbf files, but those files are not in the repository due to being greater than the maximum size of a file to upload on github. We can implement a solution by reading them from a private link on a drive, not yet implemented.
