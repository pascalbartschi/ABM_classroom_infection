###
# Group project Laura, Jule and Pascal
# ABM classroom size optimization
# from 04/04/2023 rto 30/05/2023
###

# clear environment
rm(list = ls()) 

# load tidyverse
library(tidyverse)
library(numbers)

## agents for simulation

# this function generates a student with given id, initially healthy
generate_student <- function(sid, pos = c(0, 0)){
  # create list
  student = list(sid = sid, 
                 pos = pos,
                 sick = FALSE, 
                 athome = FALSE,
                 sick_neighbours = 0)
  
  # set class
  class(student) <- "student"
  return(student)
} 

# this function generates a classroom with input id and size, 
# holds number of students of given size
generate_classroom <- function(cid, 
                               size, # classsize,
                              spacing, 
                              area# space between people in m2
                               )
  {
  # create a list classroom , which is always a sqaure
  # we defined, that space from wall is equal to space from students
  classroom = list(cid = cid, 
                   size = size,       # number of students room holds
                   spacing = spacing, # space between students
                   area = area, 
                   students = NA,
                   sick_count = 0)
  
  # set class
  class(classroom) <- "classroom"
  return(classroom)
}


# this function fills a university with students and classrooms
generate_university <- function(no_of_rooms, 
                                room_size, 
                                room_spacing){
  # initialize list
  university <- list()
  # append frame to list
  room_area <- (room_spacing * (room_size**0.5 + 2)) ** 2 # m2
  no_of_stu <- no_of_rooms * room_size
  university$frame <- list(no_of_stu = no_of_stu,
                          no_of_rooms = no_of_rooms,
                          # room_size = room_size, 
                          room_spacing = room_spacing,
                          room_area = room_area)
  
  # generate and append students to university sublist students
  university$students <- list()
  
  for (sid in 1:no_of_stu){
    university$students[[sid]] <- generate_student(sid = sid)
  }
  
  # generate and append students to university sublist classrooms
  university$classrooms <- list()
  
  for (cid in 1:no_of_rooms){
    university$classrooms[[cid]] <- generate_classroom(cid = cid, 
                                                      size = room_size, 
                                                       spacing = room_spacing, 
                                                       area = room_area)
  }
  
  # set a class title university
  class(university) <- "university"
  
  return(university)
}


fill_classroom <- function(university){
  
  # set all neighbours count to zero
  for (i_st in 1:length(university$students)){
    university$students[[i_st]]$sick_neighbours <- 0
  }
  
  # extract the frame variables
  no_of_stu <- university$frame$no_of_stu
  no_of_rooms <- university$frame$no_of_rooms
  room_size <- university$frame$room_size
  room_spacing <- university$frame$room_spacing
  
  
  ## idea: use remainder to set groups on randomly sorted ids
  # randomly sort list, set replace = F to have all once
  student_ids <- sample(1:no_of_stu, no_of_stu, replace = FALSE)
  
  # split ids in classes upon remainder grouping (%%)
  classes <- split(student_ids,
                   student_ids%%no_of_rooms)
  
  coord <- seq(0, university$frame$room_area**0.5, university$frame$room_spacing)
  coord_mask <- c(2:(length(coord)-1)) # slice first and last element (border)
  coord <- coord[coord_mask]
  xy <- expand_grid(x = coord, y= coord) # combn(x = coord, m = 2)
  # print("coord")
  # print(coord)
  
  # insert classes into classroom sublist students
  for (cid in 1:length(classes)){
    university$classrooms[[cid]]$students <- classes[[cid]]
    counter <- 0
    for (sid in classes[[cid]]){
      counter <- counter + 1
      university$students[[sid]]$pos <- as.numeric(xy[counter,])
    }
    # print(university$classroom[[i]]$students)
  }
  
  return(university)
}


update_sickness <- function(university, viral_radius = 1, beta = 0.0001){
  for (i_cr in 1:university$frame$no_of_rooms){
    # extract student indices
    students <- university$classrooms[[i_cr]]$students
    # set sick count to zero
    # university$classrooms[[i_cr]]$sickathome_count <- 0
    # count sick students in classroom

    for (i_st in students){
      for(i_neigh in students){
        pos_stu <- university$students[[i_st]]$pos
        pos_neigh <- university$students[[i_neigh]]$pos
        # print("euclidian dist")
        # print(((pos_neigh[1] - pos_stu[1])**2 + (pos_neigh[2] - pos_stu[2])**2)**0.5)
        # print("spacing")
        # print(viral_radius * university$frame$room_spacing * 2**0.5)
        # print(pos_neight, pos_stu)
        # print(c(pos_stu, pos_neigh))
        if (all(pos_neigh != pos_stu)){
          if (((pos_neigh[1] - pos_stu[1])**2 + (pos_neigh[2] - pos_stu[2])**2)**0.5 <= viral_radius){#viral_radius_factor * university$frame$room_spacing * 2**0.5){
            # delayed probability: students that went home infected people in class: reason distinguish previous sick from after class sick
            if (!university$students[[i_neigh]]$sick & university$students[[i_neigh]]$athome){
              university$students[[i_st]]$sick_neighbours <- university$students[[i_st]]$sick_neighbours + 1
            }
          }
        }
      }
    }
  }
  for (i_stu in 1:university$frame$no_of_stu){
    # calculate probability to be infected in this classroom
    p_infect <- 1 - ((1 - beta)**university$students[[i_stu]]$sick_neighbours)
    # check whether not sick and not at home to turn sick
    if (!university$students[[i_stu]]$sick & !university$students[[i_stu]]$athome){
      # draw random number in probability distribution
      r <- runif(n = 1, min = 0, max = 1)
      if (p_infect > r){
        # set student to sick if true
        university$students[[i_stu]]$sick <- TRUE
        }
      }
  }
  return(university)
}


# detect students that will stay home for the rest of the week

after_day_gohome <- function(university){
  # puts all students home that are sick
  for (i_st in 1:length(university$students)){
    # check if after class sick
    if (university$students[[i_st]]$sick){
      # if true, student goes home
      university$students[[i_st]]$athome <- TRUE
      university$students[[i_st]]$sick <- FALSE
    }
  }
  return(university)
}

student_recovery <- function(university){
  # pulls students from home back to class
  for (i_st in 1:length(university$students)){
    if (!university$students[[i_st]]$sick & university$students[[i_st]]$athome){
      university$students[[i_st]]$sick <- FALSE
      university$students[[i_st]]$athome <- FALSE
    }
  }
  return(university)
}

observe <- function(university){
  attendance <- 0
  for (i_st in 1:length(university$students)){
    # if student not at home, student is at university
    if (!university$students[[i_st]]$athome){
      attendance <- attendance + 1
    }
  }
  return(attendance)
}


simulate_university <- function(beta = 0.001,             # functional infection coef
                                viral_radius = 1,  # descides which amount of n can infect, fraction of spacing
                                no_of_rooms = 3,          # number of rooms in university
                                room_spacing = 1,         # space between students in m
                                room_size = 30,           # roomsize of classroom
                                days = 21,                # simulated days
                                classes_per_day = 3,      # representative for interactions
                                week_init_stu_ratio = 0.1 # ratio of students whick come infected from WE
                                )
  {
  
  ###
  # simulation concept:
  # a duration is simulated with given number of days and given number of classes per day
  # one week lasts seven days: after one day all students are recovered and then given random number of students are infected
  # one day has given number of classes where attendence is observed
  # one class is randomly filled, people which are sick go home, but are counted for infection probability (sick F, home T)
  # people that are infected are counted in attendance (sick T, home F) in next class infect, and go home and so on
  ###
  if (viral_radius < room_spacing){stop("Please put viral radius > room_spacing, otherwise nobody is affected")}
  else if (viral_radius == room_spacing){warning("Only direct neighbours, not diagonal neighbours can be infected.")}
  ## students
  # number
  no_of_stu <- no_of_rooms * room_size
  # sick ratio
  sick_student_start <- as.integer(no_of_stu * week_init_stu_ratio) # sick students at start of every x
  # radius factor
  # viral_radius <- viral_radius * (1/room_spacing) # normalization of the spacing factor
  
  # initialize network
  irchel <- generate_university(no_of_rooms, room_size, room_spacing)
  
  # grid to store
  day_attendance <- expand.grid(class = c(1:classes_per_day), 
                                day = c(1:days))
  
  day_attendance <- day_attendance[,c(2, 1)]
  day_attendance$attendance <- "empty"
  
  # initialize sick students
  for (s in 1:sick_student_start){
    irchel$students[[sample(1:no_of_stu, 1)]]$sick <- TRUE
  }
  
  # index for grid
  i <- 0
  for (day in 1:days){
    # print(day)
    if (day%%7 == 0){
      # after weekend, all people are healthy again
      irchel <- student_recovery(irchel)
      # students bringing sickness from the weekend
      for (s in 1:sick_student_start){
        irchel$students[[sample(1:no_of_stu, 1)]]$sick <- TRUE
      }
    }
    for (c in 1:classes_per_day){
      i <- i + 1
      # observe how many students go to class
      day_attendance$attendance[i] <- observe(irchel)
      # everyone goes to class, also ones at home
      irchel <- fill_classroom(irchel)
      # print(paste("day:",day, "class", c))
      # for (s in irchel$students){print(s$pos)}
      # go home after day if sick
      irchel <- after_day_gohome(irchel)
      # spreading desease, only people not at home are counted
      irchel <- update_sickness(irchel, viral_radius, beta)
    
    }
  }
  day_attendance <- day_attendance %>% mutate_all(., as.numeric)
  return(list(result = day_attendance,
              irchel = irchel))
}








