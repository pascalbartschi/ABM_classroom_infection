###
# Group project Laura, Jule and Pascal
# ABM classroom size optimization
# from 04/04/2023 rto 30/05/2023
###


setwd("~/Library/CloudStorage/OneDrive-Personal/Dokumente/BSc_UZH/UZH_23FS/BIO369/Project")
source("ABM_irchel_pandemic.R")

## simulation
# initialize
# model dimensions
beta <- 0.001
no_of_rooms <- 3
room_size <- 30
days <- 21
classes_per_day <- 3

View(simulate_university(beta = 0.001,
                         no_of_rooms = 3, 
                         room_size = 30,
                         days = 365, 
                         classes_per_day = 5, 
                         week_init_stu_ratio = 0.1))