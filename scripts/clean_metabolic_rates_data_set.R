#Script to clean LIRS_metabolic_rates data frame in preparation for plotting
#Written by Manoela Romano de Orte (with help from David Koweek)


#----Initialize_workspace----

#Load the LIRS_Metabolic_Rates data frame
source("combine_calcification_photosynthesis.R")

LIRS_metabolic_rates_clean <- filter (LIRS_metabolic_rates, Metabolism != "NA")

# Eliminate row Day07_Dead_Resp_4 beacuse measurement does not look good 

LIRS_metabolic_rates_clean <- filter (LIRS_metabolic_rates_clean, Replicate != "Day07_Dead_Resp_4")

# Elininate row Day08_Dead_Resp_8 because measurement does not look good

LIRS_metabolic_rates_clean <- filter (LIRS_metabolic_rates_clean, Replicate != "Day08_Dead_Resp_8")

LIRS_metabolic_rates_clean <- filter (LIRS_metabolic_rates_clean,Replicate != "Day03_Live_Resp_2") 

# Eliminate row Day07_Dead_Resp_8 because measurement is bad

LIRS_metabolic_rates_clean <- filter (LIRS_metabolic_rates_clean, Metabolism != "Resp+Photo")

# Eliminate row Day05_Dead_Resp_8

LIRS_metabolic_rates_clean <- filter (LIRS_metabolic_rates_clean, Replicate != "Day05_Dead_Resp_8")


#Fix species name
LIRS_metabolic_rates_clean <- mutate(LIRS_metabolic_rates_clean, Species = case_when((Species == "especie Favia favus") ~ "Favia favus",
                                                                                     TRUE ~ Species))

LIRS_metabolic_rates_clean <- mutate(LIRS_metabolic_rates_clean, Substrate = case_when((Substrate == "coral") ~ "Live Coral",
                                                                                       (Substrate == "algae") ~ "Algal Turf Community"))
