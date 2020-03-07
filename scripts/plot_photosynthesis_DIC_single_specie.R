#Written by Manoela Romano de Orte on May 7th

#Load the viridis colour package for later plotting
library(viridis)
source ("combine_calcification_photosynthesis.R")
source ("clean_metabolic_rates_data_set.R")


LIRS_metabolic_rates_clean <- filter (LIRS_metabolic_rates_clean, Species != "Goniastrea favulus")


LIRS_metabolic_rates_clean <- filter (LIRS_metabolic_rates_clean, Species != "Favia favus")

#Fix species name


#Add new substrate name ("Live Coral" and "Dead Coral")
#LIRS_metabolic_rates_clean <- mutate(LIRS_metabolic_rates_clean, Substrate = case_when((Substrate == "coral") ~ "Live Coral",
                                                                                      # (Substrate == "algae") ~ "Dead Coral Community"))




#LIRS_metabolic_rates_clean$Species <- factor(LIRS_metabolic_rates_clean$Species, level= c("S. recta", "G. favulus", "F. favus"))

#Here we are creating a group just for the purpose of plotting. This group combines the species name with the metabolic measurements (photosynthesis or respiration)


photosynthesis_DIC_plot <- 
  ggplot(data = LIRS_metabolic_rates_clean %>% arrange(photosynthesis_rate_DIC_mmol_h),
         mapping = aes(x = Species, 
                       y=photosynthesis_rate_DIC_mmol_h, 
                       fill = Metabolism)) +
  geom_bar (stat = "identity", 
            position = position_dodge2 (preserve = "single"), 
            colour = "black") + 
  scale_fill_viridis(name = element_blank(), 
                     labels = c("Dark","Light"),
                     discrete = TRUE) + 
  scale_y_continuous(name = expression('Net photosynthesis'~(mmol~C~m^{-2}~hr^{-1})),
                     limits = c(-10,50),
                     breaks = seq(-10,50,by=10)) +
  scale_x_discrete(name = element_blank()) +
  theme_bw() +
  #Italicize species names
  theme(axis.text.x = element_text(face = "italic")) + 
  #Remove species name
  theme(axis.text.x = element_blank()) +
  #Remove x axis ticks
  theme(axis.ticks.x = element_blank()) +
  #Increase font size on y-axis, both tick marks and label
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  #Facetting by substrate
  facet_wrap( ~ Substrate, 
              ncol = 2) 


plot(photosynthesis_DIC_plot)