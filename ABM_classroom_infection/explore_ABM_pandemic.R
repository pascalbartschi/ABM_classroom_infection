###
# Group project Laura, Jule and Pascal
# ABM classroom size optimization
# from 04/04/2023 rto 30/05/2023
###


# setwd("~/ABM_classroom_infections")
source("ABM_classroom_infection/ABM_irchel_pandemic.R")

## simulation
# initialize
# model dimensions
beta <- 0.001
no_of_rooms <- 30
room_size <- 25
room_spacing <- 1
days <- 3
classes_per_day <- 100
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

# relationship classroom size and its cost
# in next step we do need to define a function on how classroom size maps to number of places it hold
a <- 10 # m
A <- a**2 # m^2
# asssumption 1: very outer frame (1m) cannot be used, due to wall 
unused_frame_per_a <- 1
# asssumption 2: first couple of meters cannot be used, due blackboard/ teacher desk
unused_blackboard <- 3
# calculate area that is left usable


