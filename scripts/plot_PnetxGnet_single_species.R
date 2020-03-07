#Script to plot the relationships between net calcification and net photosynthesis PQ on LIRS data
#Written by Manoela Romano de Orte (with help from David Koweek) 1 March 2019

#----Initialize_workspace----

#Load necessary packages
library(viridis)
library(svglite)

#Source necessary scripts
source("clean_metabolic_rates_data_set.R")

#----Generate_plots----

calcification_photosynthesis_plot <- 
  LIRS_metabolic_rates_clean %>% 
  #Filter for a single species
  filter(Species == "Symphyllia recta") %>% 
  #Create a string that combines the substrate and metabolism measurement information
  mutate(health_by_metabolism = str_c(Substrate, Metabolism)) %>% 
  ggplot() + 
  #Set aesthetics for scatter plot
  geom_point(mapping = aes(x = photosynthesis_rate_oxygen_Q_mmol_h,
                           y = calcification_rate_mmol_h,
                           fill = Metabolism,
                           shape = Substrate),
             size = 3,
             colour = "black",
             stroke = 1) +
  stat_ellipse(aes(x = photosynthesis_rate_oxygen_Q_mmol_h,
                   y = calcification_rate_mmol_h,
                   group = health_by_metabolism)) + 
  scale_shape_manual(name = element_blank(),
                     values = c(21, 24)) +
  scale_fill_viridis(name = element_blank(), 
                     labels = c("Dark","Light"),
                     discrete = TRUE) +
  guides(fill = guide_legend(override.aes = list(shape = c(21, 24)))) +
  scale_x_continuous(name = expression(P[net]~(mmol~C~m^{-2}~hr^{-1})),
                     limits = c(-19, 53)) +
  scale_y_continuous(name = expression(G[net]~(mmol~C~m^{-2}~hr^{-1})),
                     limits = c(-7,12)) +
  #Add guide lines for Gnet = 0, Pnet = 0
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  #Add text for 4 quadrants
  annotate(geom = "text",
           label = "Production and Dissolution",
           x = 25,
           y = -6,
           size = 5) +
  annotate(geom = "text",
           label = "Production and Calcification",
           x = 25,
           y = 11,
           size = 5) +
  annotate(geom = "text",
           label = expression(atop("Respiration and", "Calcification")),
           x = -11,
           y = 11,
           size = 5) +
  annotate(geom = "text",
           label = expression(atop("Respiration and", "Dissolution")),
           x = -11,
           y = -6,
           size = 5) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

 plot(calcification_photosynthesis_plot)

#ggsave(filename = "~/Desktop/carnegie/LIRS/R_analysis/plots/Final_plots/Pnet_Gnet.svg",
 #      plot = calcification_photosynthesis_plot,
  #     width = 8,
   #    height = 6,
    #   units = "in")