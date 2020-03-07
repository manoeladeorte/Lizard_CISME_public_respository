# Script to plot delta O2 vs. Q from CISME measurements (another way to visualize Q)
# Written by Manoela Romano de Orte (with help from David Koweek)


#----Initialize_workspace----

#Source cleaned data
library(viridis)
source("clean_metabolic_rates_data_set.R")





#----Generate_plot----

#Build character lookup tables for facet lables
metabolism_table <- c(Resp = "Dark", Photo = "Light")

dO2_Q_plot <- 
  Q_df %>% 
  #Select needed data
  select(c(O2_f_mean,
           O2_i_mean,
           file)) %>% 
  #Merge with master metabolic data to acquire metadata (substrate, metabolism)
  left_join(LIRS_metabolic_rates_clean,
            .,
            by = "file") %>% 
  mutate(delta_O2 = (O2_f_mean - O2_i_mean) * 1e6) %>% #move from mol/kg to umol/kg
  ggplot(aes(x = delta_O2,
             y = Q_bar)) +
  geom_point(aes(shape = Substrate, 
                 fill = Metabolism),
             size = 3, 
             colour = "black",
             stroke = 1) +
  stat_ellipse(aes(x = delta_O2,
                   y = Q_bar,
                   group = Substrate,
                   linetype = Substrate)) +
  #Scale details
  scale_fill_viridis_d() +
  scale_shape_manual(name = element_blank(),
                     values = c(21, 24))  +
  scale_linetype_discrete(name = element_blank()) +
  scale_x_continuous(name = expression(Delta~O[2]~(mu~mol~kg^{-1}))) +
  scale_y_continuous(name = expression(Photosynthetic~Quotient~~(-Delta~DIC~"/"~Delta~O[2]))) +
  #Add reference line
  geom_hline(yintercept = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  theme(legend.text = element_text (size = 12)) +
  
  #Facet wrap by type of metabolism
  facet_wrap(~Metabolism,
             scales = "free_x",
             labeller = labeller(Metabolism = metabolism_table)) +
  theme(strip.text.x = element_text(size = 14))

plot(dO2_Q_plot)