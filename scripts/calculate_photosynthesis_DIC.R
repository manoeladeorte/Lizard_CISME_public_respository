# Script to calculate DIC based photosynthesis

# Written by Manoela Romano de Orte (with help from David Koweek) on April 18 2019

#----Initialize_workspace----

#Load necessary packages
library(tidyverse)
library(seacarb)


#Source necessary scripts
source("add_and_clean_CISME_oxygen_pH.R") 
source("calculate_calcification.R") # We are going to use the calcification values to calculate photosynthesis based on DIC
source("averaging_interval.R") #sets the interval of which beginning and end measurements are averaged
source("CISME_dimensions.R")

#----Calculate_DIC_from_TA_and_pH_data----

CISME_oxygen_pH_data_sets_cleaned <- 
  mutate(CISME_oxygen_pH_data_sets_cleaned,
         DIC_i = carb(flag = 8,
                      var1 = CISME_pH,
                      var2 = Alk_mini_i_imputed * 1e-6, #alkalinity needs to be in mol/kg, not umol/kg
                      S = Salinity_imputed,
                      T = CISME_temperature)[["DIC"]], #In mols/kg
         DIC_f = carb(flag = 8,
                      var1 = CISME_pH,
                      var2 = Alk_mini_f * 1e-6, #alkalinity needs to be in mol/kg, not umol/kg
                      S = Salinity_imputed,
                      T = CISME_temperature)[["DIC"]],
         Omega_Ar_i = carb(flag = 8,
                           var1 = CISME_pH,
                           var2 = Alk_mini_i_imputed * 1e-6, #alkalinity needs to be in mol/kg, not umol/kg
                           S = Salinity_imputed,
                           T = CISME_temperature)[["OmegaAragonite"]],
         Omega_Ar_f = carb(flag = 8,
                           var1 = CISME_pH,
                           var2 = Alk_mini_f * 1e-6, #alkalinity needs to be in mol/kg, not umol/kg
                           S = Salinity_imputed,
                           T = CISME_temperature)[["OmegaAragonite"]]
  ) %>% 
  rename(file = CISME_filename) #rename for later merge with calcification data

#----Select_DIC_data_for_photosynthesis_calculations----

data_for_photosynthesis_DIC <- 
  mutate(CISME_oxygen_pH_data_sets_cleaned,
         DIC_i = case_when(CISME_elapsed_time - CISME_t_i < avg_interval ~ DIC_i),
         DIC_f = case_when(CISME_t_f - CISME_elapsed_time < avg_interval ~ DIC_f),
         Omega_Ar_i = case_when(CISME_elapsed_time - CISME_t_i < avg_interval ~ Omega_Ar_i),
         Omega_Ar_f = case_when(CISME_t_f - CISME_elapsed_time < avg_interval ~ Omega_Ar_f)) %>% # Since we only one the DIC values that 
  #correspond to pH initial and alk initial or pH final and alk final we eliminate the values of DIC final that correspond to pH initial and vice versa
  
  #For each filename (which corresponds to an individual set of metabolism estimates)
  group_by(file) %>% 
  summarize(DIC_i = mean(DIC_i, na.rm = TRUE),
            DIC_f = mean(DIC_f, na.rm = TRUE),
            Omega_Ar_i = mean(Omega_Ar_i, na.rm = TRUE),
            Omega_Ar_f = mean(Omega_Ar_f, na.rm = TRUE))

#----Merge_DIC_data_with_calcifcation_data_frame----
LIRS_photosynthesis_DIC <- 
  #Join with LIRS calcification data frame 
  left_join(LIRS_calcification,
            data_for_photosynthesis_DIC,
            by = "file") %>% 
  mutate(
        delta_DIC = DIC_i - DIC_f, delta_Arag = Omega_Ar_i - Omega_Ar_f, #mol/kg
         
         photosynthesis_rate_DIC = ((seawater_density * CISME_loop_volume * delta_DIC) / 
                                      (CISME_head_area * delta_time)) - calcification_rate, #mol C/m2/s,
         photosynthesis_rate_DIC_mmol_h = photosynthesis_rate_DIC * 3.6e6 #mol/m2/s -> mmol/m2/hr                   
        )

