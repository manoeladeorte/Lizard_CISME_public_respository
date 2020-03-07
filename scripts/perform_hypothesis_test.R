
# script to perform t-test in calcification and photosynthesis between Algal turf community and live corals

source("clean_metabolic_rates_data_set.R")

# make a new table selecting only columns that we need for the statistic

statistic_table  <-
select(LIRS_metabolic_rates_clean, Metabolism, Substrate, Species, photosynthesis_rate_oxygen_Q_mmol_h, calcification_rate_mmol_h, Q_bar) %>% 
  filter (Species == "Symphyllia recta") 

#t-test and Wilcoxon rank sum test for photosynthesis between algal turf and live corals during daytime

photosynthesis_day_test <- statistic_table %>% filter(Metabolism == "Photo") %>% t.test(photosynthesis_rate_oxygen_Q_mmol_h ~ Substrate, data = .)
photosynthesis_day_test_nonparametric <- statistic_table %>% filter(Metabolism == "Photo") %>% wilcox.test(photosynthesis_rate_oxygen_Q_mmol_h ~ Substrate, data = .)

#t-test and Wilcoxon rank sum test for photosynthesis between algal turf and live corals during nighttime

photosynthesis_night_test <- statistic_table %>% filter(Metabolism == "Resp") %>% t.test(photosynthesis_rate_oxygen_Q_mmol_h ~ Substrate, data = .)
photosynthesis_night_test_nonparametric <- statistic_table %>% filter(Metabolism == "Resp") %>% wilcox.test(photosynthesis_rate_oxygen_Q_mmol_h ~ Substrate, data = .)


#t-test and Wilcoxon rank sum test for calcification between algal turf and live corals during daytime 

calcification_day_test <- statistic_table %>% filter(Metabolism == "Photo") %>% t.test(calcification_rate_mmol_h ~ Substrate, data = .)
calcification_day_test_nonparametric <- statistic_table %>% filter(Metabolism == "Photo") %>% wilcox.test(calcification_rate_mmol_h ~ Substrate, data = .)


#t-test and Wilcoxon rank sum test for calcification between algal turf and live corals during nighttime 

calcification_night_test <- statistic_table %>% filter(Metabolism == "Resp") %>% t.test(calcification_rate_mmol_h ~ Substrate, data = .)
calcification_night_test_nonparametric <- statistic_table %>% filter(Metabolism == "Resp") %>% wilcox.test(calcification_rate_mmol_h ~ Substrate, data = .)

#t-test and Wilcoxon rank sum test for photosynthesic/respiratory  quotient (Q) between algal turf and live corals during daytime

Q_day_test <- statistic_table %>% filter(Metabolism == "Photo") %>% t.test(Q_bar ~ Substrate, data = .)
Q_day_test_nonparametric <- statistic_table %>% filter(Metabolism == "Photo") %>% wilcox.test(Q_bar ~ Substrate, data = .)


#t-test and Wilcoxon rank sum test for photosynthesic/respiratory  quotient (Q) between algal turf and live corals during nighttime

Q_night_test <- statistic_table %>% filter(Metabolism == "Resp") %>% t.test(Q_bar ~ Substrate, data = .)
Q_night_test_nonparametric <- statistic_table %>% filter(Metabolism == "Resp") %>% wilcox.test(Q_bar ~ Substrate, data = .)