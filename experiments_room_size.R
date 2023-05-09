###
# Group project Laura, Jule and Pascal
# ABM classroom size optimization experiments
# from 04/04/2023 to 30/05/2023
###


# setwd("~/ABM_classroom_infections")
source("ABM_irchel_pandemic.R")


beta <- 1e-1      # only magnitude of .1 seems realistic
no_of_rooms <- 8  # controls total number of classrooms
# room_size <- 25   # hyperparameter, square number please
room_spacing <- 1.0 # hyperparameter: MUST BE <=1
days <- 28    # controls number of students
classes_per_day <- 1 # controls randomness
week_init_stu_ratio <- 0.03  # how many students come in sick from weekend
viral_radius <- 2**0.5# viral radius relative to room spacing hardcode to become vrf * 1/spacing

filename <- "room_size_16to49"
values <- c(4**2, 5**2, 6**2)
number_of_values <- length(values)
number_of_sims_per_value <- 10

########################## DO NOT CHANGE ANYTHING BELOW HERE ######################################

# if (!filename){stop("Please provide a filename!")}

storage_simulations <- expand.grid(class = c(1:classes_per_day), 
                                  day = c(1:days))

storage_matrix_mean_day <- matrix(nrow = number_of_values, ncol = number_of_sims_per_value, NA)
storage_matrix_mean_week <- matrix(nrow = number_of_values, ncol = number_of_sims_per_value, NA)
storage_matrix_areas <- matrix(nrow = 1, ncol = number_of_values)

for (i in 1:number_of_values) {
  # simulate of couple of times to later take mean
  temp_store_sim <- expand.grid(class = c(1:classes_per_day), day = c(1:days))
  for (j in 1:number_of_sims_per_value) {
  
    sim <- simulate_university(beta = beta,
                               viral_radius = viral_radius,
                               no_of_rooms = no_of_rooms, 
                               room_size = values[i],
                               room_spacing = room_spacing,
                               days = days, 
                               classes_per_day = classes_per_day, 
                               week_init_stu_ratio = week_init_stu_ratio)
    
    temp_store_sim <- cbind(sim$result$attendance, temp_store_sim)
    colnames(temp_store_sim)[1] <- c(paste0("sim_", j))
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
  means <- apply(X = as.matrix(temp_store_sim %>% select(starts_with("sim"))),
                MARGIN = c(1), 
                FUN = mean)
  stds <- apply(X = as.matrix(temp_store_sim %>% select(starts_with("sim"))),
                MARGIN = c(1), 
                FUN = sd)
  
  storage_simulations <- cbind(means, stds, storage_simulations)
  colnames(storage_simulations)[1:2] <- c(paste0("sim_", values[i], "_mean"), paste0("sim_", values[i], "_std"))
  storage_matrix_areas[1,i] <- sim$irchel$classrooms[[1]]$area
}


value_means_day <- apply(X = storage_matrix_mean_day, 
                         MARGIN = c(1), 
                         FUN = mean)

value_means_week <- apply(X = storage_matrix_mean_week, 
                         MARGIN = c(1), 
                         FUN = mean)

summary_df <- data.frame(value = values, 
              mean_week = value_means_week,
              mean_day = value_means_day, 
              area = storage_matrix_areas[1,])


sim_results <- list(sim_store = storage_simulations,
                    summary = summary_df, 
                    values = values,
                    file = filename)

dir <- "experiments"
if (!dir.exists(dir)){dir.create(dir)}

saveRDS(object = sim_results, 
        file.path(dir, 
                  paste0("results_", filename, ".RDS")))

print(paste0("Output saved as results_", filename, " in directory ", dir))

# names(value_means_day) <- names(value_means_week) <- values_room_spacing




