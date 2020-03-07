#Script to plot calcification data for only one specie
#Written by Manoela Romano de Orte (with help from David Koweek)

#Load the viridis colour package for later plotting
library(viridis)
source ("clean_metabolic_rates_data_set.R")


calcification_plot <- 
  LIRS_metabolic_rates_clean %>% 
  #Using only Symphyllia recta as the primary species
  filter(Species == "Symphyllia recta") %>% 
  #Arrange the data by increasing calcification rate
  arrange(calcification_rate_mmol_h) %>% 
  ggplot(mapping = aes(x = Species,
                       fill = Metabolism)) +
  geom_bar(aes(y=calcification_rate_mmol_h),
           stat = "identity", 
           position = position_dodge2(),
           width = 0.9,
           colour = "black") + 
  geom_errorbar(aes(ymin = calcification_rate_mmol_h - calcification_rate_SE_mmol_h,
                    ymax = calcification_rate_mmol_h + calcification_rate_SE_mmol_h),
                width = 0.9,
                position =  position_dodge2(width = 0.9) 
                ) +
  scale_fill_viridis(name = element_blank(), 
                     labels = c("Dark","Light"),
                     discrete = TRUE) +
  scale_y_continuous(name = expression('Net calcification'~(mmol~C~m^{-2}~hr^{-1}))) +
  scale_x_discrete(name = element_blank()) +
  theme_bw() +
  #Italicize species names
  # theme(axis.text.x = element_text(face = "italic")) + 
  #Remove species name
  theme(axis.text.x = element_blank()) +
  #Remove x axis ticks
  theme(axis.ticks.x = element_blank()) +
  #Increase font size on y-axis, both tick marks and label
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(legend.text = element_text (size = 12)) +
  #Facetting by substrate
  facet_wrap( ~ Substrate, 
              ncol = 2) +
  #Increase plot title font size
  theme(strip.text.x = element_text(size = 14))


plot(calcification_plot)






