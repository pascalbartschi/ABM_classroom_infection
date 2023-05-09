###
# Group project Laura, Jule and Pascal
# ABM classroom size optimization experiments
# from 04/04/2023 to 30/05/2023
###

library(tidyverse)

results <- readRDS("experiments/results_room_size_16to49.RDS")

# reasemble for plotting
mean_app <- results$sim_store %>% 
  select(ends_with("mean"), day, class) %>% 
  gather(key = "type", value = "appereance", -c(day, class))
