# Script to open up CISME dissolved oxygen (O2) and pH data and clean the data set
# O2 data will be used for CISME photosynthesis measurements
# pH data will be combined with discrete alkalinity measurements to calculate DIC and then photosynthesis

#----Initialize_workspace----

#Source necessary scripts
source(here::here("scripts", "add_CISME_temperature.R"))

#----Grab_CISME_data----
#Set up lists to collect data (later will be combined into a single data frame)
data <- list()

#Open up each CISME file and grab the necessary information
for (i in 1:nrow(LIRS_master_data)) {
  
  #Use this "??f" statement to skip the rows without files
  if (LIRS_master_data[["file"]][i] != "") {
    
    #Create the full path to each file
    CISME_file <- str_c(individual_files_path,
                        "/",
                        LIRS_master_data[["file"]][i])
    
    #Open each file
      #wrap in 'supressWarnings' to eliminate parsing error warnings on each file
    suppressWarnings(
      CISME_data <- 
        read_csv(file = CISME_file,
                 col_types = c("ddddddddddddd"),
                 skip = 20)
    )
    
    #Grab the necessary information from the CISME file to place it into our data frame
    CISME_elapsed_time <- 
      CISME_data[["Time(m)"]]
    
    CISME_oxygen <-
      CISME_data[["O2 (umol/kg)"]]
    
    CISME_pH <- 
      CISME_data[["ISFET pH"]]
    
    CISME_filename <- 
      LIRS_master_data[["file"]][i]
    
    CISME_t_i <- 
      LIRS_master_data[["t_model_i"]][i]
    
    CISME_t_f <-
      LIRS_master_data[["t_model_f"]][i]
    
    CISME_temperature <-
      LIRS_master_data[["Temperature"]][i]
    
    Alk_mini_i_imputed <- 
      LIRS_master_data[["Alk_mini_i_imputed"]][i]
    
    Alk_mini_f <- 
      LIRS_master_data[["Alk_mini_f"]][i]
    
    Salinity_imputed <- 
      LIRS_master_data[["Salinity_imputed"]][i]
    
    Substrate <- 
      LIRS_master_data[["Substrate"]][i]
    
    Metabolism <- 
      LIRS_master_data[["Metabolism"]][i]
    
    Date <- 
      LIRS_master_data[["Date"]][i]
    
    #Combine the information into a data frame and put the entire data frame into a list element (part of a list)
    data[[i]] <- data.frame(CISME_elapsed_time = CISME_elapsed_time,
                            CISME_oxygen = CISME_oxygen,
                            CISME_pH = CISME_pH,
                            CISME_filename = CISME_filename,
                            CISME_t_i = CISME_t_i,
                            CISME_t_f = CISME_t_f,
                            CISME_temperature = CISME_temperature,
                            Alk_mini_i_imputed = Alk_mini_i_imputed,
                            Alk_mini_f = Alk_mini_f,
                            Salinity_imputed = Salinity_imputed,
                            Substrate = Substrate,
                            Metabolism = Metabolism,
                            Date = Date)
    
  }
}

#Combine list elements into a data frame
CISME_oxygen_pH_data_sets <- 
  plyr::ldply(data,
              data.frame) %>% 
  tbl_df()

#----Generate_diagnostic_plots----

#Generate exploratory plots of CISME O2 without filtering the start and stop time of each measurement
CISME_O2_ts_all <- 
  ggplot(data = CISME_oxygen_pH_data_sets,
         aes(x = CISME_elapsed_time,
             y = CISME_oxygen)) +
  geom_line() +
  facet_wrap(~CISME_filename)

#Now let's filter out the times not between the start and stop times of each measurement
CISME_O2_ts_filtered <- 
  CISME_oxygen_pH_data_sets %>% 
  filter(CISME_elapsed_time > CISME_t_i & CISME_elapsed_time < CISME_t_f) %>% 
  ggplot(aes(x = CISME_elapsed_time,
             y = CISME_oxygen)) +
  geom_line() +
  facet_wrap(~CISME_filename)

#----Clean_CISME_data_set----
#So filtered data looks exceptionally good. There was one measurement that combined...
#...photosynthesis and respiration. Eliminate it before fitting regressions to the individual data sets

filename_to_remove <- 
  LIRS_master_data %>% 
  filter(Metabolism == "Resp+Photo") %>% 
  slice(2) %>% 
  pull(file)

CISME_oxygen_pH_data_sets_cleaned <-
  CISME_oxygen_pH_data_sets %>% 
  filter(CISME_filename != filename_to_remove,
         CISME_elapsed_time > CISME_t_i,
         CISME_elapsed_time < CISME_t_f)