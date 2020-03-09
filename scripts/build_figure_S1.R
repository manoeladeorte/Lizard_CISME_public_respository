#Script to plot calcification rates from all CISME incubations


#----Initialize_workspace----

source(here::here("scripts", "clean_metabolic_rates_data_set.R"))

#----Build_plot----

calcification_plot_all_species <-
  LIRS_metabolic_rates_clean %>% 
  mutate(species_metabolism = str_c(Species, Metabolism, sep = " ")) %>% 
  arrange(calcification_rate_mmol_h) %>% 
  ggplot(mapping = aes(x = Species, 
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


#----Export_plot----
ggsave(filename = here::here("output", "figures", "figure_S1.pdf"),
       plot = calcification_plot_all_species,
       height = 6,
       width = 8,
       units = "in")

