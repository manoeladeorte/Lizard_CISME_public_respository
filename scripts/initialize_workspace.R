# Install "pacman"
# install.packages("pacman")

# Load all necessary packages here and check for conflicts
pacman::p_load(here,
               tidyverse,
               seacarb,
               broom,
               naniar,
               lubridate,
               readxl,
               viridis,
               cowplot)


#Define path for the individual CISME files
individual_files_path <- 
  here::here("data", "CISME_incubation_files")

#Define the master data file for CISME incubations
CISME_master_data <- 
  here::here("data","Data_Information 181113.xlsx")

#Define the data file for total alkalinity measurements
TA_master_data <- 
  here::here("data", "Alkalinity Calculator _Lizard_10312018.xls")

#Load information about the CISME volume and surface area for rate estimates
source(here::here("scripts", "CISME_dimensions.R"))