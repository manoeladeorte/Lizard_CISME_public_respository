#Conduct tests of significance for different metabolic variables...
#...(net calcification, net photosynthesis, and the metabolic quotient)...
#...between different substrate groups (live coral vs. algal turf community)

#----Initialize_workspace----

source(here::here("scripts", "clean_metabolic_rates_data_set.R"))

#Make a new table selecting only columns that we need for the statistic
statistic_table  <-
  select(
    LIRS_metabolic_rates_clean,
    Metabolism,
    Substrate,
    Species,
    photosynthesis_rate_oxygen_Q_mmol_h,
    calcification_rate_mmol_h,
    Q_bar
  ) %>%
  filter (Species == "Symphyllia recta") 

#----Conduct_significance_tests----

#t-test and Wilcoxon rank sum test for photosynthesis between algal turf and live corals during daytime

photosynthesis_day_test <-
  statistic_table %>% 
  filter(Metabolism == "Photo") %>% 
  t.test(photosynthesis_rate_oxygen_Q_mmol_h ~ Substrate, data = .)

photosynthesis_day_test_nonparametric <- 
  statistic_table %>% 
  filter(Metabolism == "Photo") %>% 
  wilcox.test(photosynthesis_rate_oxygen_Q_mmol_h ~ Substrate, data = .)

#t-test and Wilcoxon rank sum test for photosynthesis between algal turf and live corals during nighttime

photosynthesis_night_test <- 
  statistic_table %>% 
  filter(Metabolism == "Resp") %>% 
  t.test(photosynthesis_rate_oxygen_Q_mmol_h ~ Substrate, data = .)

photosynthesis_night_test_nonparametric <- 
  statistic_table %>% 
  filter(Metabolism == "Resp") %>% 
  wilcox.test(photosynthesis_rate_oxygen_Q_mmol_h ~ Substrate, data = .)

#t-test and Wilcoxon rank sum test for calcification between algal turf and live corals during daytime 

calcification_day_test <-
  statistic_table %>% 
  filter(Metabolism == "Photo") %>% 
  t.test(calcification_rate_mmol_h ~ Substrate, data = .)

calcification_day_test_nonparametric <- 
  statistic_table %>% 
  filter(Metabolism == "Photo") %>% 
  wilcox.test(calcification_rate_mmol_h ~ Substrate, data = .)

#t-test and Wilcoxon rank sum test for calcification between algal turf and live corals during nighttime 

calcification_night_test <- 
  statistic_table %>% 
  filter(Metabolism == "Resp") %>% 
  t.test(calcification_rate_mmol_h ~ Substrate, data = .)

calcification_night_test_nonparametric <- 
  statistic_table %>% 
  filter(Metabolism == "Resp") %>% 
  wilcox.test(calcification_rate_mmol_h ~ Substrate, data = .)

#t-test and Wilcoxon rank sum test for metabolic quotient (Q) between algal turf and live corals during daytime

Q_day_test <- 
  statistic_table %>% 
  filter(Metabolism == "Photo") %>% 
  t.test(Q_bar ~ Substrate, data = .)

Q_day_test_nonparametric <- 
  statistic_table %>% 
  filter(Metabolism == "Photo") %>% 
  wilcox.test(Q_bar ~ Substrate, data = .)

#t-test and Wilcoxon rank sum test for photosynthesic/respiratory  quotient (Q) between algal turf and live corals during nighttime

Q_night_test <- 
  statistic_table %>% 
  filter(Metabolism == "Resp") %>% 
  t.test(Q_bar ~ Substrate, data = .)

Q_night_test_nonparametric <- 
  statistic_table %>% 
  filter(Metabolism == "Resp") %>% 
  wilcox.test(Q_bar ~ Substrate, data = .)


#----Export_results----

tests <- 
   list(photosynthesis_day_test,
        photosynthesis_day_test_nonparametric,
        photosynthesis_night_test,
        photosynthesis_day_test_nonparametric,
        calcification_day_test,
        calcification_day_test_nonparametric,
        calcification_night_test,
        calcification_night_test_nonparametric,
        Q_day_test,
        Q_day_test_nonparametric,
        Q_night_test,
        Q_night_test_nonparametric)

test_titles <- 
      c("Daytime net photosynthesis",
        "Daytime net photosynthesis (non-parametric)",
        "Nighttime net photosynthesis",
        "Nighttime net photosynthesis (non-parametric)",
        "Daytime net calcificiation",
        "Daytime net calcification (non-parameteric)",
        "Nighttime net calcification",
        "Nighttime net calcification (non-parametric)",
        "Daytime metabolic quotient",
        "Daytime metabolic quotient (non-parameteric)",
        "Nighttime metabolic quotient",
        "Nighttime metabolic quotient (non-parametric)")

results_file <- 
  here::here("output", "significance_tests", "test_outputs.txt")

cat("Tests of significance", file = results_file)

for (i in 1:length(tests)) {
  cat("\n\n\n", 
      file = results_file, 
      append = TRUE)
  
  cat(test_titles[i],
      file = results_file,
      append = TRUE)
  
  cat("\n", 
      file = results_file, 
      append = TRUE)
  
  capture.output(tests[[i]],
                 file = results_file,
                 append = TRUE)
  
}
