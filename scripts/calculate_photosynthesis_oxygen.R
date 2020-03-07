# Script to calculate photosynthesis and respiration from CISME oxygen data


# Written by Manoela Romano de Orte (with help from David Koweek)


#----Initialize_workspace----

#Load necessary packages
library(tidyverse)
library(seacarb)
library(broom) # package for cleaning linear regression results

#Source necessary scripts
source("add_and_clean_CISME_oxygen_pH.R")
source("CISME_dimensions.R")

#----Calculate_photosynthesis_and_respiration----


#Calculate seawater density
CISME_oxygen_pH_data_sets_cleaned <- 
  CISME_oxygen_pH_data_sets_cleaned %>% 
  mutate(seawater_density = swRho(salinity = Salinity_imputed, #kg/m??3
                                  temperature = CISME_temperature,
                                  pressure = 0))

#Run a linear regression through each oxygen time series to calculate dO2/dt
oxygen_regressions <-  
  #For each CISME file...
  CISME_oxygen_pH_data_sets_cleaned %>% 
  nest(-CISME_filename) %>% 
  #Fit a linear regression to the oxygen data as a function of time
  #Then clean up the output
  mutate(fit = map(data, ~lm(CISME_oxygen ~ CISME_elapsed_time,
                             data = .x)),
         cleaned_fits = map(fit, broom::tidy)) %>% 
  unnest(cleaned_fits) %>% 
  #And grab statistics on the slope (while ignoring the intercept (odd rows))
  slice(seq(2, nrow(.), by = 2))

#Now combine the oxygen regression data with the original data set
CISME_oxygen_pH_data_sets_cleaned <- 
  #Take the full data set with all of the CISME oxygen measurements for all the files
  CISME_oxygen_pH_data_sets_cleaned %>% 
  #Chunk the data by filename
  nest(-CISME_filename) %>% 
  #Join it to the oxygen regression data
  left_join(oxygen_regressions,
            by = "CISME_filename") %>% 
  #Unpack the data
  unnest() %>% 
  #Save the slope and error estimate on the slope as new variables
  dplyr::mutate(dO2_dt = estimate, #umol/kg/min
         SE_dO2_dt = std.error,
         SE_dO2_dt = SE_dO2_dt * 1e-6 * (1/60), #umol/kg/min -> mol/kg/s
         #Calculate Photosynthesis as P = (rho*V/A)*dO2/dt
         photosynthesis_rate_oxygen = ((seawater_density * CISME_loop_volume) / 
                                        CISME_head_area) * dO2_dt,  #umol/m2/min
         photosynthesis_rate_oxygen = photosynthesis_rate_oxygen * 1e-6 * (1/60), #convert to mol/m2/s
         photosynthesis_rate_oxygen_SE = abs(seawater_density * CISME_loop_volume / CISME_head_area) * SE_dO2_dt, #mol/m2/s
         photosynthesis_rate_oxygen_mmol_h = photosynthesis_rate_oxygen * 3.6e6, #mol/m2/s -> mmol/m2/hr  
         photosynthesis_rate_oxygen_SE_mmol_h = photosynthesis_rate_oxygen_SE * 3.6e6) #mmol/m2/hr 

#And let's define a new data frame with the photosynthesis rates and leaving out all of the oxygen measurements
LIRS_photosynthesis_oxygen <- 
  CISME_oxygen_pH_data_sets_cleaned %>% 
  select(c(Date,
           Metabolism,
           Substrate,
           photosynthesis_rate_oxygen,
           photosynthesis_rate_oxygen_mmol_h,
           photosynthesis_rate_oxygen_SE,
           photosynthesis_rate_oxygen_SE_mmol_h,
           CISME_filename)) %>%  
  dplyr::rename(file = CISME_filename) %>% 
  dplyr::mutate(file = as.character(file)) %>% 
  distinct()
  