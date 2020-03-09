#Plot to compare net photosynthesis estimates from DIC and TA anomalies vs. ...
#.... times series of O2 and metabolic quotients

#----Initialize_workspace----

source(here::here("scripts", "clean_metabolic_rates_data_set.R"))

#----Build_plot----

photosynthesis_Q_plot <- 
  LIRS_metabolic_rates_clean %>% 
  filter(Species == "Symphyllia recta") %>% 
  arrange(photosynthesis_rate_oxygen_Q_mmol_h) %>% 
  ggplot(mapping = aes(x = Species, 
                       y=photosynthesis_rate_oxygen_Q_mmol_h, 
                       fill = Metabolism)) + 
  geom_bar (stat = "identity", 
            position = position_dodge2 (preserve = "single"), 
            colour = "black") + geom_errorbar(aes(ymin = photosynthesis_rate_oxygen_Q_mmol_h - photosynthesis_rate_oxygen_Q_SE_mmol_h,
                                                  ymax = photosynthesis_rate_oxygen_Q_mmol_h + photosynthesis_rate_oxygen_Q_SE_mmol_h),
                                              width = 0.9,
                                              position =  position_dodge2(width = 0.9) ) +
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


#----Export_plot----

ggsave(filename = here::here("output", "figures", "figure_S2b.pdf"),
       plot = photosynthesis_Q_plot,
       height = 8,
       width = 8,
       units = "in")