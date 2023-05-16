###
# Group project Laura, Jule and Pascal
# ABM classroom size optimization experiments
# from 04/04/2023 to 30/05/2023
###

## load libraries and results
library(tidyverse)
library(ggplot2)

results_room <- readRDS("experiments/results_room_size_4to196_100sims.RDS")
results_spacing <- readRDS("experiments/results_room_spacing_0.1to1by0.1_100sim.RDS")

## data wrangling for plotting
# room variation
mean_app <- results_room$sim_store %>% 
  select(ends_with("mean"), day, class) %>% 
  gather(key = "mean_val", value = "mean_appereance", -c(day, class))

mean_sd <- results_room$sim_store %>% 
  select(ends_with("std"), day, class) %>% 
  gather(key = "std_val", value = "sd_appereance", -c(day, class))

plt_res_room <- cbind(mean_app, mean_sd %>% select(-c(day, class))) %>%
  mutate("room_size" = sub("sim_(\\d+)_mean", "\\1 people", mean_val))

# spacing variation
mean_app <- results_spacing$sim_store %>% 
  select(ends_with("mean"), day, class) %>% 
  gather(key = "mean_val", value = "mean_appereance", -c(day, class))

mean_sd <- results_spacing$sim_store %>% 
  select(ends_with("std"), day, class) %>% 
  gather(key = "std_val", value = "sd_appereance", -c(day, class))

plt_res_spacing <- cbind(mean_app, mean_sd %>% select(-c(day, class))) %>% 
  mutate("room_spacing" = sub("sim_(\\d+)_mean", "spacing \\1", mean_val))

## first plot: show dynamics of model
fig1 <- plt_res_spacing %>%
  filter(mean_val == "sim_0.6_mean") %>%
  ggplot(mapping = aes(x = day, y = mean_appereance)) + 
  geom_errorbar(aes(ymin = mean_appereance - sd_appereance, 
                    ymax = mean_appereance + sd_appereance), 
                width = 0.2) + 
  geom_ribbon(mapping = aes(x = day, ymin = mean_appereance - sd_appereance,
                            ymax = mean_appereance + sd_appereance),
              alpha = 0.3, color = "grey") +
  geom_point() + 
  labs(x = "day", y = expression(bar(n))) +
  geom_line(linetype = "dashed", alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 28, by = 7)) +
  theme_bw()

## second plot: compare dynamics of spacing and room size
# A: sizes 16, 25, 49
fig2a <- plt_res_room %>%
  filter(mean_val %in% paste0("sim_", c(16, 25, 49), "_mean")) %>%
  ggplot(mapping = aes(x = day, y = mean_appereance, color = room_size)) + 
  geom_errorbar(aes(ymin = mean_appereance - sd_appereance, ymax = mean_appereance + sd_appereance), 
                width = 0.2, alpha = 0.3) + 
  geom_point() + 
  labs(x = "day", y = expression(bar(appereance)), color = "room size") +
  geom_line(linetype = "dashed", alpha = 0.5) + 
  theme_bw()

# B: spacings 0.3, 0.6, 0.9
fig2b <- plt_res_spacing %>%
  filter(mean_val %in% paste0("sim_", c(0.3, 0.6, 0.9), "_mean")) %>%
  ggplot(mapping = aes(x = day, y = mean_appereance, color = room_spacing)) + 
  geom_errorbar(aes(ymin = mean_appereance - sd_appereance, ymax = mean_appereance + sd_appereance), 
                width = 0.2, alpha = 0.3) + 
  geom_point() + 
  labs(x = "day", y = expression(bar(appereance)), color = "room spacing") +
  geom_line(linetype = "dashed", alpha = 0.5) + 
  theme_bw()

## figure 3: infection per area at different room spacing and room sizes
# mean per week per m^2 vs area
fig3a <- results_room$summary %>%
  mutate(area = seq(2, 14, by = 1)**2 * 8) %>% # area per room * rooms
  ggplot(mapping = aes(x = area, y = mean_week / area)) + 
  geom_errorbar(mapping = aes(ymin = mean_week / area - sd_week / area,
                              ymax = mean_week / area + sd_week / area),
                width = 0.2) +
  labs(x = "area", y = expression(bar(infections) / week / area)) +
  geom_point() + 
  theme_bw()

# mean per week per person vs area
fig3b <- results_spacing$summary %>%
  filter(area > 2) %>%
  ggplot(mapping = aes(x = area, y = mean_week / area)) + 
  geom_errorbar(mapping = aes(ymin = mean_week / area - sd_week / area,
                              ymax = mean_week / area + sd_week / area),
                width = 0.2) +
  labs(x = "area", y = expression(bar(infections) / week / person)) +
  geom_point()



