# Script to load the master primary data for the Lizard Island CISME data set

#----Initialize_workspace----

source(here::here("scripts", "initialize_workspace.R"))

#----Load_the_primary_data_file----

LIRS_master_data <- 
  read_xlsx(path = CISME_master_data, 
            sheet = "Planilha1", 
            col_names = c("Replicate", #Here we define the names of each of the columns as we want them
                          "Filename",
                          "Date",
                          "t_alk_i",
                          "Alk_robo",
                          "Alk_mini_i",
                          "Alk_mini_i_imputed",
                          "t_alk_f",
                          "Alk_mini_f",
                          "Substrate",
                          "Metabolism",
                          "Species",
                          "Salinity",
                          "Salinity_imputed",
                          "t_model_i",
                          "t_model_f",
                          "Notes"),
            col_types = c("text", #Here we tell R how to treat the data in each column as it is opened
                          "text",
                          "text",
                          "text",
                          "numeric",
                          "numeric",
                          "numeric",
                          "text",
                          "numeric",
                          "text",
                          "text",
                          "text",
                          "numeric",
                          "numeric",
                          "numeric",
                          "numeric",
                          "text"),
            na = "no_file", #Replace "no_file" with NA
            skip = 1) #Skip the first line in the Excel file since is is full of column headers

