#Plot to compare net photosynthesis estimates from DIC and TA anomalies vs. ...
#.... times series of O2 and metabolic quotients

#----Initialize_workspace----

source(here::here("scripts", "clean_metabolic_rates_data_set.R"))

#----Build_photosynthesis_DIC_plot----

photosynthesis_DIC_plot <- 
  LIRS_metabolic_rates_clean %>% 
  filter(Species == "Symphyllia recta") %>% 
  mutate(Metabolism = factor(Metabolism, levels = c("Resp", "Photo"))) %>% 
  arrange(photosynthesis_rate_DIC_mmol_h) %>% 
  ggplot(mapping = aes(x = Species, 
                       y=photosynthesis_rate_DIC_mmol_h, 
                       fill = Metabolism)) +
  geom_bar (stat = "identity", 
            position = position_dodge2 (preserve = "single"), 
            colour = "black") + 
  scale_fill_viridis(name = element_blank(), 
                     labels = c("Dark","Light"),
                     discrete = TRUE) + 
  scale_y_continuous(name = expression(Net~photosynthesis~or~respiration~(mmol~C~m^{-2}~hr^{-1})),
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



#----Build_photosynthesis_Q_plot----
photosynthesis_Q_plot <- 
  LIRS_metabolic_rates_clean %>% 
  filter(Species == "Symphyllia recta") %>% 
  mutate(Metabolism = factor(Metabolism, levels = c("Resp", "Photo"))) %>% 
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
  scale_y_continuous(name = expression(Net~photosynthesis~or~respiration~(mmol~C~m^{-2}~hr^{-1})),
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
summary_plot <- 
  plot_grid(photosynthesis_DIC_plot,
            photosynthesis_Q_plot,
            align = "hv",
            labels = "AUTO")

cowplot::ggsave2(filename = here::here("output", "figures", "figure_S3.pdf"),
       plot = summary_plot,
       height = 5.5,
       width = 12,
       units = "in")