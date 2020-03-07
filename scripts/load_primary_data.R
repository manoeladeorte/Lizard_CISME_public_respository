# Script to load the master primary data for the Lizard Island CISME data set
# Written by Manoela Romano de Orte (with help from David Koweek)

#----Initialize_workspace----

#Loading the necessary packages
library(tidyverse)
library(readxl)

#Define the path
primary_data_path <- "/Users/manoela/Desktop/carnegie/LIRS/cisme_analysis/data.information/"

#Define the most up-to-date primary data file
primary_data_file <- "Data_Information 181113.xlsx"

#Define the primary data file
filename <- str_c(primary_data_path,
                  primary_data_file,
                  sep= "")

#----Load_the_primary_data_file----

LIRS_master_data <- 
  read_xlsx(path = filename, #This is the filename of the data set. We defined this filename by concatenating the path and the file (see above)
            sheet = "Planilha1", #This is where we define the sheet in the Excel workbook to open
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

