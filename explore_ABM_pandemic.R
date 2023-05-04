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
beta <- 0.001
no_of_rooms <- 3
room_size <- 25
room_spacing <- 1
days <- 14
classes_per_day <- 3
week_init_stu_ratio <- 0.1
radius_factor <- 1

sim <- simulate_university(beta = beta,
                           radius_factor = radius_factor,
                           no_of_rooms = no_of_rooms, 
                           room_size = room_size,
                           room_spacing = room_spacing,
                           days = days, 
                           classes_per_day = classes_per_day, 
                           week_init_stu_ratio = week_init_stu_ratio)

sim %>%
mutate_at(c("day", "class", "attendance"), as.numeric) %>%
filter(class == 1) %>%
select(-class) %>%
ggplot() + 
geom_path(mapping = aes(x = day, y = attendance)) +
theme_bw()


