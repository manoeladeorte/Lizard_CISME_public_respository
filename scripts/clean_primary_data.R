# Script to clean the master primary data for the Lizard Island CISME data set

#----Initialize_workspace----

#Source the script to load the data
source(here::here("scripts", "load_primary_data.R"))

#----Load_raw_total_alkalinity_data----
#Load the data sheet of alkalinity runs for later use (see below)

# #Sheet of dates and day_sample_IDs
# TA_data_sheet_path <- 
#   "/Users/manoela/Desktop/carnegie/LIRS/cisme_analysis/ankalinity measurements/Mini TA/"
# 
# TA_data_sheet_filename <- 
#   "Alkalinity Calculator _Lizard_10312018.xls"

TA_data_sheet <- 
  read_xls(path = TA_master_data,
           sheet = "sample_date")

#Summary information for daily instrumental precision
TA_precision_sheet <- 
  read_xls(path = TA_master_data,
           sheet = "Precision") %>% 
  rename(Run_Date = X__1, 
         St_Dev = DAILY,
         Replicates = Samples) %>% 
  select(c(Run_Date,
           St_Dev,
           Replicates)) %>%
  tail(-1) %>%   #Removes first row of data frame
  mutate(St_Dev = as.numeric(St_Dev)) #coerce character vector to numeric

#----Clean_the_data_set----

#Replace missing values with NA
LIRS_master_data <-
  replace_with_na(LIRS_master_data,
                  replace = list(t_alk_i = 0,
                                 Alk_robo = 0.00,
                                 Alk_mini_i = 0.00,
                                 Alk_mini_i_imputed = 0.00,
                                 t_alk_f = 0,
                                 Alk_mini_f = 0.00)
                  )

#Convert the data values from text to dates
LIRS_master_data <- 
  LIRS_master_data %>% 
  mutate(Date = str_c(Date, "2018"),
         Date = as.Date(Date, "%b%d%Y")
         )

#Convert the time values from text to time

  #First, we need to pad the zeros on the hours < 12 
for (i in 1:nrow(LIRS_master_data)) {
  
  #If the row is not NA and the measurements start before 10:00 am
  if(is.na(LIRS_master_data[["t_alk_i"]][i]) == FALSE & as.numeric(LIRS_master_data[["t_alk_i"]][i]) < 1000) {
    
    LIRS_master_data[["t_alk_i"]][i] <- str_c("0",
                                              LIRS_master_data[["t_alk_i"]][i])
    
  }
  
  #If the row is not NA and the measurements end before 10:00 am
  if(is.na(LIRS_master_data[["t_alk_f"]][i]) == FALSE & as.numeric(LIRS_master_data[["t_alk_f"]][i]) < 1000) {
    
    LIRS_master_data[["t_alk_f"]][i] <- str_c("0",
                                              LIRS_master_data[["t_alk_f"]][i])
    
  }
  
}

  #Now that the times are represented uniformly, we can convert the text to time
LIRS_master_data <-
  mutate(LIRS_master_data, t_alk_i = as.POSIXct(str_c(Date, t_alk_i),
                                              format = "%Y-%m-%d %k%M"),
                           t_alk_f = as.POSIXct(str_c(Date, t_alk_f), format = "%Y-%m-%d %H%M"))

#Remove the path dependency on the filenames
LIRS_master_data <- 
  LIRS_master_data %>% 
  mutate(file = str_split_fixed(Filename, 
                                pattern = "Data/", 
                                n = 2)[,2])



#----Merge_alkalinity_error_information----

#Extract the alkalinity precision and match up with the LIRS master data file to know the alkalinity precision on each date

#1. Extract the string of sample date and CISME run from the LIRS_master_data data frame
# (eventually to be used to know run date of TA bottle samples)
LIRS_master_data <-
  LIRS_master_data %>%
  mutate(day_number = str_extract(string = Replicate,
                                  pattern = "[1-9]+"),
         sample_number_daily = str_extract(string = Replicate,
                                           pattern = "\\d$"),
         day_number = case_when((as.numeric(day_number) == 1) ~ "10",
                                         TRUE ~ day_number),
         day_sample_ID = str_c("D",
                               day_number,
                               "_R",
                               sample_number_daily,
                               sep = "")) %>%
  select(-c(day_number,
            sample_number_daily))

#2. Extract the day_sample_ID from the TA sheet
TA_data_sheet <-
  TA_data_sheet %>%
  mutate(day_number = str_extract(string = replicate,
                                  pattern = "[1-9]+"),
         sample_number_daily = str_extract(string = replicate,
                                           pattern = "\\d$"),
         day_sample_ID = str_c("D",
                               day_number,
                               "_R",
                               sample_number_daily,
                               sep = "")) %>%
          select(-c(replicate,
                    ID_sample,
                    day_number,
                    sample_number_daily
                    ))

#Manually correct "D1" to "D10
TA_data_sheet <-
  TA_data_sheet %>%
  mutate(day_sample_ID = case_when((grepl(pattern = "D1_R3", x = day_sample_ID)) ~ "D10_R3",
         TRUE ~ day_sample_ID),
         day_sample_ID = case_when((grepl(pattern = "D1_R4", x = day_sample_ID)) ~ "D10_R4",
                                   TRUE ~ day_sample_ID))

#3. Join LIRS_master_data and TA_data_sheet using the 'day_sample_ID' column
LIRS_master_data <-
  LIRS_master_data %>%
  left_join(.,
            TA_data_sheet,
            by = "day_sample_ID")

#4. Grab the standard deviation and number of replicates of standard from each day
LIRS_master_data <- 
  LIRS_master_data %>% 
  left_join(.,
            TA_precision_sheet %>% 
              rename(Initial_date = Run_Date),
            by = "Initial_date") %>% 
  rename(Initial_St_Dev = St_Dev,
         Initial_Replicates = Replicates) %>% 
  left_join(.,
            TA_precision_sheet %>% 
              rename(Final_date = Run_Date),
            by = "Final_date") %>% 
  rename(Final_St_Dev = St_Dev,
         Final_Replicates = Replicates)
