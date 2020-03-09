# Script to pair CISME temperature measurements with master data set

#----Initialize_workspace----

#Load cleaned primary data set
source(here::here("scripts", "clean_primary_data.R"))

# #Define path for the individual CISME files
# 
# individual_files_path <- "/Users/manoela/Desktop/carnegie/LIRS/cisme_analysis/Data_treatment_ken/CISME_data_all/"

#Pre-define temperature column
LIRS_master_data <- 
  mutate(LIRS_master_data, Temperature = NA)


#----Open_each_file_and_add_temperature----

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
  
  
  #Take the mean temperature and record it in the LIRS_master_data data frame
  LIRS_master_data[["Temperature"]][i] <- 
    mean(CISME_data[["ISFET Temp"]],
         na.rm = TRUE)
  
  }
  
}

