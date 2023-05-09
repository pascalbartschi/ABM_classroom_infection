###
# Group project Laura, Jule and Pascal
# ABM classroom size optimization experiments
# from 04/04/2023 to 30/05/2023
###

library(tidyverse)

results <- readRDS("experiments/results_room_spacing_0.3to0.9by0.3.RDS")

# reasemble for plotting
mean_app <- results$sim_store %>% 
  select(ends_with("mean"), day, class) %>% 
  gather(key = "mean_val", value = "mean_appereance", -c(day, class))

mean_sd <- results$sim_store %>% 
  select(ends_with("std"), day, class) %>% 
  gather(key = "std_val", value = "sd_appereance", -c(day, class))


cbind(mean_app, mean_sd %>% select(-c(day, class))) %>%
  ggplot(mapping = aes(x = day, y = mean_appereance, color = mean_val)) + 
  geom_errorbar(aes(ymin = mean_appereance - sd_appereance, ymax = mean_appereance + sd_appereance), 
                width = 0.2) + 
  geom_point() + 
  geom_line(linetype = "dashed", alpha = 0.5)

ggplot(data = results$summary, mapping = aes(x = area, y = mean_week / area)) + 
  geom_errorbar(mapping = aes(ymin = mean_week / area - sd_week / area, 
                              ymin = mean_week / area - sd_week / area),
                width = 0.2)
  labs(x = "area", y = "mean infections per week per area")
  geom_point()



