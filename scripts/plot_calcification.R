#Script to clean LIRS_metabolic_rates data frame in preparation for plotting
#Written by Manoela Romano de Orte (with help from David Koweek)

#Load the viridis colour package for later plotting
library(viridis)
source ("combine_calcification_photosynthesis.R")
source ("clean_metabolic_rates_data_set.R") 


LIRS_metabolic_rates_clean$Species <- factor(LIRS_metabolic_rates_clean$Species, level= c("Symphyllia recta", "Goniastrea favulus", "Favia favus"))


LIRS_metabolic_rates_clean <- 
  LIRS_metabolic_rates_clean %>% 
  mutate(species_metabolism = str_c(Species, Metabolism, sep = " "))

calcification_plot <- 
  ggplot(data = LIRS_metabolic_rates_clean %>% arrange(calcification_rate_mmol_h),
         mapping = aes(x = Species, 
                       y=calcification_rate_mmol_h, 
                       fill = Metabolism,
                       group = species_metabolism)) + 
  geom_bar (stat = "identity", 
            position = position_dodge2 (preserve = "single"), 
            width = 0.9,
            colour = "black") + 
  scale_fill_viridis(name = element_blank(), 
                     labels = c("Dark","Light"),
                     discrete = TRUE) +
  scale_y_continuous(name = expression('Net calcification'~(mmol~C~m^{-2}~hr^{-1}))) +
  theme_bw() +
  #Italicize species names
  theme(axis.text.x = element_text(face = "italic")) + 
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(legend.text = element_text (size = 12)) +
  theme(strip.text.x = element_text(size = 14)) +
  #Facetting by substrate
  facet_wrap( ~ Substrate, 
              ncol = 2) 


plot(calcification_plot)



