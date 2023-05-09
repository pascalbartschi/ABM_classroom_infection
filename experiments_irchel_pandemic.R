###
# Group project Laura, Jule and Pascal
# ABM classroom size optimization experiments
# from 04/04/2023 to 30/05/2023
###


# setwd("~/ABM_classroom_infections")
source("ABM_irchel_pandemic.R")


beta <- 1e-1      # only magnitude of .1 seems realistic
no_of_rooms <- 8  # controls total number of classrooms
room_size <- 25   # hyperparameter, square number please
# room_spacing <- 1.0 # hyperparameter: MUST BE <=1
days <- 28    # controls number of students
classes_per_day <- 1 # controls randomness
week_init_stu_ratio <- 0.03  # how many students come in sick from weekend
viral_radius <- 2**0.5# viral radius relative to room spacing hardcode to become vrf * 1/spacing

number_of_values <- 10
values_room_spacing <- seq(0.1, 1, length = 10)
number_of_sims_per_value <- 10

storage_matrix_mean_day <- matrix(nrow = number_of_values, ncol = number_of_sims_per_value, NA)
storage_matrix_mean_week <- matrix(nrow = number_of_values, ncol = number_of_sims_per_value, NA)

for (i in 1:number_of_values) {
  for (j in 1:number_of_sims_per_value) {
  
    sim <- simulate_university(beta = beta,
                               viral_radius = viral_radius,
                               no_of_rooms = no_of_rooms, 
                               room_size = room_size,
                               room_spacing = values_room_spacing[i],
                               days = days, 
                               classes_per_day = classes_per_day, 
                               week_init_stu_ratio = week_init_stu_ratio)
    # daily mean of infections
    storage_matrix_mean_day[i, j] <- sim$result %>%
      mutate_at(c("day", "class", "attendance"), as.numeric) %>%
      filter(class == 1) %>%
      select(-class) %>%
      mutate(infected_per_day = ifelse(c(0, diff(attendance)) > 0, 0, -c(0, diff(attendance)))) %>%
      filter(day %% 7 != 0) %>%
      summarise(mean_day = mean(infected_per_day)) %>%
      pull(.)
    
    # weekly mean of infections
    storage_matrix_mean_week[i, j] <- sim$result %>%
      mutate_at(c("day", "class", "attendance"), as.numeric) %>%
      filter(class == 1) %>%
      select(-class) %>%
      filter(day == 1 | day %% 7 == 0 | day %% 7 == 6) %>%
      mutate(infected_per_week = ifelse(c(0, diff(attendance)) > 0, 0, -c(0, diff(attendance)))) %>%
      filter(infected_per_week != 0) %>% 
      summarise(mean_week = mean(infected_per_week)) %>%
      pull(.)
    
  }
}

value_means_day <- apply(X = storage_matrix_mean_day, 
                         MARGIN = c(1), 
                         FUN = mean)

value_means_week <- apply(X = storage_matrix_mean_week, 
                         MARGIN = c(1), 
                         FUN = mean)

names(value_means_day) <- names(value_means_week) <- values_room_spacing




