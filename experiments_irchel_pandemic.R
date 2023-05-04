###
# Group project Laura, Jule and Pascal
# ABM classroom size optimization experiments
# from 04/04/2023 to 30/05/2023
###


# setwd("~/ABM_classroom_infections")
source("ABM_irchel_pandemic.R")

# params / ranges
beta <- 0.001 # fixed
no_of_rooms <- 30
room_size <- 25
room_spacing <- 1
days <- 3
classes_per_day <- 3
week_init_stu_ratio <- 0.1
radius_factor <- 1


for (spacing_factor in c(1:3)){
  for (spacing in c(1:3)){
    #simulate
  }
}