#Script to clean LIRS_metabolic_rates data frame in preparation for plotting

#----Initialize_workspace----

source(here::here("scripts", "combine_calcification_photosynthesis.R")) #Load 'LIRS_metabolic_rates' data frame

#----Remove_faulty_incubations----

#Remove missing data
LIRS_metabolic_rates_clean <- 
  LIRS_metabolic_rates %>% 
  filter(Metabolism != "NA")

#Eliminate cases where leakage was suspected
LIRS_metabolic_rates_clean <-
  LIRS_metabolic_rates_clean %>% 
  filter(!(Replicate %in% c("Day07_Dead_Resp_4",
                            "Day08_Dead_Resp_8",
                            "Day03_Live_Resp_2",
                            "Day05_Dead_Resp_8"))) %>% 
  filter(Metabolism != "Resp+Photo")

#----Adjust_species_names----

#Fix species name
LIRS_metabolic_rates_clean <- 
  LIRS_metabolic_rates_clean %>% 
  mutate(Species = case_when((Species == "especie Favia favus") ~ "Favia favus", 
                             TRUE ~ Species))

LIRS_metabolic_rates_clean <- 
  LIRS_metabolic_rates_clean %>% 
  mutate(Substrate = case_when((Substrate == "coral") ~ "Live Coral",
                               (Substrate == "algae") ~ "Algal Turf Community"))
