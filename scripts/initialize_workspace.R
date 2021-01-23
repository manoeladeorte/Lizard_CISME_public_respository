

# Load all necessary packages here and check for conflicts
library(here)
library(tidyverse)
library(seacarb)
library(broom)
library(naniar)
library(lubridate)
library(readxl)
library(viridis)
library(cowplot)


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