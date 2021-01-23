#Script to plot calcification rates from all CISME incubations


#----Initialize_workspace----

source(here::here("scripts", "clean_metabolic_rates_data_set.R"))

#----Build_plot----

photosynthesis_plot_all_species <-
  LIRS_metabolic_rates_clean %>% 
  mutate(Metabolism = factor(Metabolism, levels = c("Resp", "Photo"))) %>% 
  mutate(Species = factor(Species, levels = c("Symphyllia recta",
                                              "Goniastrea favulus",
                                              "Favia favus"))) %>% 
  mutate(species_metabolism = str_c(Species, Metabolism, sep = " ")) %>% 
  mutate(species_metabolism = factor(species_metabolism, levels = c("Symphyllia recta Resp",
                                                                    "Symphyllia recta Photo",
                                                                    "Goniastrea favulus Resp",
                                                                    "Goniastrea favulus Photo",
                                                                    "Favia favus Resp",
                                                                    "Favia favus Photo"))) %>% 
  arrange(photosynthesis_rate_oxygen_Q_mmol_h) %>% 
  ggplot(mapping = aes(x = Species, 
                       y=photosynthesis_rate_oxygen_Q_mmol_h, 
                       fill = Metabolism,
                       group = species_metabolism)) + 
  geom_bar (stat = "identity", 
            position = position_dodge2 (preserve = "single"), 
            width = 0.9,
            colour = "black") + 
  scale_fill_viridis(name = element_blank(), 
                     labels = c("Dark","Light"),
                     discrete = TRUE) +
  scale_y_continuous(name = expression(Net~photosynthesis~or~respiration~(mmol~C~m^{-2}~hr^{-1}))) +
  scale_x_discrete(labels = c("S. recta", "G. favulus", "F. favus")) +
  theme_bw() +
  #Italicize species names
  theme(axis.text.x = element_text(face = "italic",
                                   size = 14),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  theme(legend.text = element_text (size = 12)) +
  theme(strip.text.x = element_text(size = 14)) +
  #Facetting by substrate
  facet_wrap( ~ Substrate, 
              ncol = 2) 


#----Export_plot----
ggsave(filename = here::here("output", "figures", "figure_S2.pdf"),
       plot = photosynthesis_plot_all_species,
       height = 6,
       width = 8,
       units = "in")

