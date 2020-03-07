# Script to calculate delta alkalinity for the Lizard Island CISME data set
# Written by Manoela Romano de Orte


#----Initialize_workspace----

#Load necessary packages
library(seacarb)

#Source the script to load the data set with appended CISME temperature readings, Temperature needed to calculate density
source("add_CISME_temperature.R")

#Source script with area and volume measurements of the CISME unit
source("CISME_dimensions.R")


#----Calculate_calcification----



#Eliminate rows in data set missing key measurements for calcification rate calculations
LIRS_calcification <- 
  filter(LIRS_master_data,
         !is.na(t_alk_i) & 
         !is.na(t_alk_f))

#Calculate calcification rate and necessary parameters beforehand
LIRS_calcification <- 
  dplyr::mutate(LIRS_calcification,
         delta_alk = Alk_mini_i_imputed - Alk_mini_f, #umol/kg
         delta_alk = delta_alk / 1e6, #convert from umol/kg to mol/kg
         delta_alk_SE = sqrt(
           (((Initial_St_Dev / 1e6) / sqrt(Initial_Replicates)) ^ 2) +
             (((Final_St_Dev / 1e6) / sqrt(Final_Replicates)) ^ 2)
         ), #mol/kg
         coefficient_of_variation = delta_alk_SE / delta_alk,
         seawater_density = swRho(salinity = Salinity_imputed, #kg/m3
                                  temperature = Temperature,
                                  pressure = 0),
         seawater_density = case_when(is.na(seawater_density) ~ 1023,
                                      TRUE ~ seawater_density),
         delta_time = t_alk_f - t_alk_i, #mins
         delta_time = delta_time * 60, #seconds
         delta_time = as.numeric(delta_time), #convert from a 'difftime' object to a number for division
         calcification_rate = (seawater_density * CISME_loop_volume * delta_alk) / (2 * CISME_head_area * delta_time),  #mol/m??2/s
         calcification_rate_mmol_h = calcification_rate *3.6e6, # mol/m2/s -> mmol/m2/hr                                                                            
         calcification_rate_SE = abs((seawater_density * CISME_loop_volume) / 
                                       (2 * CISME_head_area * delta_time)) *
                                  delta_alk_SE, #mol/m2/s
         calcification_rate_SE_mmol_h = calcification_rate_SE * 3.6e6 # mol/m2/s -> mmol/m2/hr
           )  