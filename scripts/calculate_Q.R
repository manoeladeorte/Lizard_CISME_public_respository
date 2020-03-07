# Script to calculate the net respiratory quotient following Barnes et al. 1983
# Use pH, O2, and TA measurements to invert Barnes Eq. 9 to solve for Q
# Written by Manoela Romano de Orte (with help from David Koweek)

#----Initialize_workspace----

#Load necessary packages
library(tidyverse)
#seacarb loaded when "calculate_photosynthesis_DIC.R" script is sourced

#Source necessary scripts
source("calculate_photosynthesis_DIC.R") #(sources calculated DIC needed for Barnes equation)
source("averaging_interval.R") #sets the interval of which beginning and end measurements are averaged

#Define custom function for taking standard error of all non-NA values in group
std_error <- function(x) {
  
  n <- sum(!is.na(x))
  sigma <- sd(x, na.rm =TRUE)
  std_error <- sigma / sqrt(n)
  
  return(std_error)
}


#----Calculate_hydroxide_alkalinity
Q_df <- 
  CISME_oxygen_pH_data_sets_cleaned %>% 
  mutate(H = 10^(-CISME_pH), #mol/kg-soln (total scale)
         K_water = Kw(S = Salinity_imputed, #mol^2/kg^2 (total scale)
                      T = CISME_temperature,
                      pHscale = "T"), 
         HA = K_water / H #hydroxide alkalinity (OH) mol/kg
  )

#----Calculate_borate_alkalinity
Q_df <- 
  Q_df %>% 
  mutate(total_boron = bor(S = Salinity_imputed), #mol/kg
         K_boron = Kb(S = Salinity_imputed, #mol/kg
                      T = CISME_temperature,
                      pHscale = "T"),
         BOH3 = (H * total_boron) / (K_boron + H), #mol/kg
         BA = total_boron - BOH3 #B(OH)4 (mol/kg)
         ) 

#----Calculate_K_in_Barnes_equation----

#K in Barnes Eqs. 3-9 is defined as K = DIC/carbonate alkalinity

Q_df <- 
  Q_df %>% 
  mutate(K_i = DIC_i / (Alk_mini_i_imputed/1e6 - BA - HA),
         K_f = DIC_f / (Alk_mini_f/1e6 - BA - HA)
  )

#----Select_data_for_Q_calculations----

Q_df <- 
  Q_df %>% 
  mutate(K_i = case_when(CISME_elapsed_time - CISME_t_i < avg_interval ~ K_i),
         K_f = case_when(CISME_t_f - CISME_elapsed_time < avg_interval ~ K_f),
         BA_i = case_when(CISME_elapsed_time - CISME_t_i < avg_interval ~ BA),
         BA_f = case_when(CISME_t_f - CISME_elapsed_time < avg_interval ~ BA),
         HA_i = case_when(CISME_elapsed_time - CISME_t_i < avg_interval ~ HA),
         HA_f = case_when(CISME_t_f - CISME_elapsed_time < avg_interval ~ HA),
         O2_i = case_when(CISME_elapsed_time - CISME_t_i < avg_interval ~ CISME_oxygen),
         O2_f = case_when(CISME_t_f - CISME_elapsed_time < avg_interval ~ CISME_oxygen)
         ) %>% # Since we only have the one K values that 
  #correspond to pH initial and alk initial or pH final and alk final we eliminate the values of K final that 
  #correspond to pH initial and vice versa
  # HOwever, for BA and HA we dont have initial and final values yet so we are taking the initial time interval and the
  #final time interval and transforming
  #BA and HA data into BA initial and BA final and same with HA
  #For each filename (which corresponds to an individual set of metabolism estimates)
  group_by(file) %>% 
  summarize_at(c("Alk_mini_i_imputed",
                 "Alk_mini_f",
                 "O2_i", 
                 "O2_f",
                 "K_i",
                 "K_f",
                 "BA_i",
                 "BA_f",
                 "HA_i",
                 "HA_f"), 
               funs(mean(.,na.rm = TRUE), 
                    std_error(.)
                    )
               ) %>% 
  rename(TA_i_mean = Alk_mini_i_imputed_mean,
         TA_f_mean = Alk_mini_f_mean) 

#Now merge standard error estimates from discrete alkalinity samples in the LIRS_calcification df
Q_df <- 
  Q_df %>% 
  left_join(.,
            LIRS_calcification,
            by = "file") %>% 
  mutate(TA_i_std_error = Initial_St_Dev / sqrt(Initial_Replicates),
         TA_f_std_error = Final_St_Dev / sqrt(Final_Replicates)) %>% 
  #only keep pertinent columns
  select(c(file,
           contains("_mean"),
           contains("_std_error")),
         -c(Alk_mini_i_imputed_std_error,
            Alk_mini_f_std_error)) %>% 
  #move umol/kg to mol/kg for consistency
  mutate(TA_i_mean = TA_i_mean / 1e6,
         TA_f_mean = TA_f_mean / 1e6,
         O2_i_mean = O2_i_mean / 1e6,
         O2_f_mean = O2_f_mean / 1e6,
         TA_i_std_error = TA_i_std_error / 1e6,
         TA_f_std_error = TA_f_std_error / 1e6,
         O2_i_std_error = O2_i_std_error / 1e6,
         O2_f_std_error = O2_f_std_error / 1e6)
  
 

#----Calculate_Q----

#Perform Monte Carlo simulations to generate mean estimates of Q with SE 
n_iterations <- 1e4

#Pre-allocate list for all Monte Carlo results of Q
Q <- list()

for (i in 1:nrow(Q_df)) {
  
  #For reproducibility
  set.seed(1234)
  
  #Define the draw of each variable
  TA_i <-
    rnorm(n = n_iterations,
          mean = Q_df[["TA_i_mean"]][i],
          sd = Q_df[["TA_i_std_error"]][i])
  
  TA_f <-
    rnorm(n = n_iterations,
          mean = Q_df[["TA_f_mean"]][i],
          sd = Q_df[["TA_f_std_error"]][i])
  
  BA_i <-
    rnorm(n = n_iterations,
          mean = Q_df[["BA_i_mean"]][i],
          sd = Q_df[["BA_i_std_error"]][i])
  
  BA_f <-
    rnorm(n = n_iterations,
          mean = Q_df[["BA_f_mean"]][i],
          sd = Q_df[["BA_f_std_error"]][i])
  
  HA_i <-
    rnorm(n = n_iterations,
          mean = Q_df[["HA_i_mean"]][i],
          sd = Q_df[["HA_i_std_error"]][i])
  
  HA_f <-
    rnorm(n = n_iterations,
          mean = Q_df[["HA_f_mean"]][i],
          sd = Q_df[["HA_f_std_error"]][i])
  
  K_i <-
    rnorm(n = n_iterations,
          mean = Q_df[["K_i_mean"]][i],
          sd = Q_df[["K_i_std_error"]][i])
  
  K_f <-
    rnorm(n = n_iterations,
          mean = Q_df[["K_f_mean"]][i],
          sd = Q_df[["K_f_std_error"]][i])
  
  O2_i <-
    rnorm(n = n_iterations,
          mean = Q_df[["O2_i_mean"]][i],
          sd = Q_df[["O2_i_std_error"]][i])
  
  O2_f <-
    rnorm(n = n_iterations,
          mean = Q_df[["O2_f_mean"]][i],
          sd = Q_df[["O2_f_std_error"]][i])
  
  
  #Calculate the distribution of Q values (calculation is vectorized)
  Q[[i]] <- 
             (
               ((K_f - 0.5) * (TA_i - TA_f)) - 
                 (K_i * (BA_i + HA_i)) + 
                 (K_f * (BA_f + HA_f)) - 
                 (TA_i * (K_f - K_i))
             ) / 
             (O2_f - O2_i)
    
  
}

#Attach Monte Carlo Results to original data frame
#Standard deviation of distribution of means is the standard error in the data set
Q_df <- 
  Q_df %>% 
  mutate(Q = Q,
         Q_bar = sapply(Q, mean, simplify = TRUE), 
         Q_SE = sapply(Q, sd, simplify = TRUE),
         Q_CV = Q_SE / Q_bar)
