###
# Group project Laura, Jule and Pascal
# ABM classroom size optimization
# from 04/04/2023 rto 30/05/2023
###

library(ggplot2)

# setwd("~/ABM_classroom_infections")
source("ABM_irchel_pandemic.R")

## simulation
# initialize
# model dimensions
beta <- 1e-1      # only magnitude of .1 seems realistic
no_of_rooms <- 4  # controls total number of classrooms
room_size <- 25   # hyperparameter, square number please
room_spacing <- 0.9 # hyperparameter: MUST BE <=1
days <- 31       # controls number of students
classes_per_day <- 3 # controls randomness
week_init_stu_ratio <- 0.01  # how many students come in sick from weekend
viral_radius <- 2**0.5# viral radius relative to room spacing hardcode to become vrf * 1/spacing


sim <- simulate_university(beta = beta,
                           viral_radius = viral_radius,
                           no_of_rooms = no_of_rooms, 
                           room_size = room_size,
                           room_spacing = room_spacing,
                           days = days, 
                           classes_per_day = classes_per_day, 
                           week_init_stu_ratio = week_init_stu_ratio)

# sim$result %>%
#   mutate_at(c("day", "class", "attendance"), as.numeric) %>%
#   filter(class == 1) %>%
#   select(-class) %>%
#   mutate(athome_per_day = ifelse(c(0, diff(attendance)) > 0, 0, -c(0, diff(attendance)))) %>%
#   ggplot() +
#   geom_path(mapping = aes(x = day, y = athome_per_day), linetype = "dashed") +
#   geom_point(mapping = aes(x = day, y = athome_per_day)) +
#   theme_bw()
# 
sim$result %>%
  mutate_at(c("day", "class", "attendance"), as.numeric) %>%
  filter(class == 1) %>%
  select(-class) %>%
  mutate(athome_per_day = ifelse(c(0, diff(attendance)) > 0, 0, -c(0, diff(attendance)))) %>%
  ggplot() +
  geom_path(mapping = aes(x = day, y = attendance), linetype = "dashed") +
  geom_point(mapping = aes(x = day, y = attendance)) +
  theme_bw()



