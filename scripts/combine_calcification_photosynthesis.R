# Combine calcification and photosynthesis rate estimates into a single data frame
# Written by Manoela Romano de Orte (with help from David Koweek)


#----Initialize_workspace----

#Source calcification and photosynthesis data sets
source("calculate_photosynthesis_DIC.R") #this script already sources the calcification rate calculations
source("calculate_photosynthesis_oxygen.R")
source("calculate_Q.R")

#Set seed for later Monte Carlo generation of Pnet_O2 distributions
set.seed(1234)

#----Wrangle_rate_estimates_into_one_data_frame----

#Wrangle large calcification and DIC-based photosynthesis data set in preparation for data merge
calcification_DIC_photosynthesis_rates <- 
  LIRS_photosynthesis_DIC %>% 
  filter(Metabolism != "Resp+Photo") %>% 
  select(c(Date,
           Metabolism,
           Substrate,
           Species,
           calcification_rate,
           calcification_rate_mmol_h,
           calcification_rate_SE,
           calcification_rate_SE_mmol_h,
           photosynthesis_rate_DIC,
           photosynthesis_rate_DIC_mmol_h,
           file,
           Notes,
           Replicate))


LIRS_metabolic_rates <-
  #Join the data sets by matching the CISME file name between the data sets
  full_join(LIRS_photosynthesis_oxygen,
            calcification_DIC_photosynthesis_rates,
            by = "file") %>% 
  #Remove columns introduced during the merge
  select(-c(Date.y,
            Metabolism.y,
            Substrate.y)) %>% 
  #Join the Q estimates to the metabolic rate estimates
  full_join(.,
            Q_df,
            by = "file") %>% 
  #Remove columns introduced during the merge
  select(-c(TA_i_mean:TA_f_std_error)) %>% 
  #Calculate CO2-based photosynthesis from O2 and Q
    #First, generate Monte Carlo distribution of Pnet_O2 from mean and standard error
    #Second, multiply Monte Carlo distributions of Q and Pnet_O2
    #Third, take mean and standard error of the new distribution
  rowwise() %>% 
  #Generating the distribution of Pnet_O2 values
  mutate(Pnet_O2_distribution = list(rnorm(n = 10000,
                                      mean = photosynthesis_rate_oxygen,
                                      sd = photosynthesis_rate_oxygen_SE)))
  
  #Can't seem to join distributions of products of Q and Pnet_O2 to the primary data frame using the dplyr pipe chain...
  photosynthesis_rate_oxygen_Q_dist <- list()
  photosynthesis_rate_oxygen_Q <- c()
  photosynthesis_rate_oxygen_Q_SE <- c()
  
  for (i in 1:nrow(LIRS_metabolic_rates)) {
    
    #Generating the distribution of Pnet_CO2 values -This is were we define photosynthesis PQ, wuich is photosynthesis_oxygen * Q, here we are multiplying the distribution 
    photosynthesis_rate_oxygen_Q_dist[[i]] <-
      LIRS_metabolic_rates[["Pnet_O2_distribution"]][[i]] * 
      LIRS_metabolic_rates[["Q"]][[i]]
    
    #Taking the mean from each distribution
    photosynthesis_rate_oxygen_Q[i] <- 
      sapply(photosynthesis_rate_oxygen_Q_dist[i],
             mean,
             na.rm = TRUE)
    
    #Taking the standard deviation from each distribution (noting that the s.d. of a distribution of means is the standard error)
    photosynthesis_rate_oxygen_Q_SE[i] <- 
      sapply(photosynthesis_rate_oxygen_Q_dist[i],
             sd,
             na.rm =TRUE)
  }
  
#Return to pipe chain to merge new results with data frame and do units conversion (to mmol/m2/hr)        
LIRS_metabolic_rates <- 
  LIRS_metabolic_rates %>% 
  bind_cols(tibble(photosynthesis_rate_oxygen_Q_dist),
            tibble(photosynthesis_rate_oxygen_Q),
            tibble(photosynthesis_rate_oxygen_Q_SE)
            ) %>% 
  mutate(photosynthesis_rate_oxygen_Q_mmol_h = photosynthesis_rate_oxygen_Q * 3.6e6, #mol CO2/m^2/s -> mmol CO2/m^2/hr
         photosynthesis_rate_oxygen_Q_SE_mmol_h = photosynthesis_rate_oxygen_Q_SE * 3.6e6) %>%  #mol CO2/m^2/s -> mmol CO2/m^2/hr 
  #Rename the columns to remove the ".x" added during the merge
  dplyr::rename(Date = Date.x,
                Metabolism = Metabolism.x,
                Substrate = Substrate.x) %>% 
  #Remove list columns to decrease memory
  select(-c(Q,
            Pnet_O2_distribution,
            photosynthesis_rate_oxygen_Q_dist))