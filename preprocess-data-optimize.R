
require(readr)
require(dplyr)
require('Matrix')


# function to prepare data for model 
preprocess_data <- function(year, loc, gender){
  # year is string of the desired year
  # loc is string of the location of test-run folder
  # gender is string of either "M" or "F"
  
  # read in data
  athlete_information <- read_csv(paste(loc,"test-run/data/",year,"/athlete-information-",year,".csv",sep=""))
  individual_performance <- read_csv(paste(loc,"test-run/data/",year,"/individual-performance-",year,".csv",sep=""), 
                                     col_types = cols(pace = col_character(), backup_name =  col_character(), backup_team =  col_character()))
  college_information <- read_csv(paste(loc,"test-run/data/college-team-information.csv",sep=""))
  
  # Select events and population
  college_athletes <- athlete_information %>% 
    filter(team_gender == gender,
           #delete this . > 
           team_name %in% (college_information %>% filter(team_name=="Pomona-Pitzer") %>% distinct(team_name) %>% pull()))
  # add filter for athletes on teams!
  perf <- individual_performance %>% 
    filter(!is.na(place), athlete_id %in% (college_athletes %>% pull(athlete_id)))
  events <- perf %>% group_by(event_id) %>%
    summarize(count = n()) %>% filter(count > 4) %>%
    pull(event_id)
  
  sorted_ids <- perf %>% filter(event_id %in% events) %>%
    distinct(athlete_id) %>% 
    pull(athlete_id) %>% sort()
  n = length(sorted_ids)
  
  
  # create the W and X matrices for each event result
  # look at paper for clear definitions of W and X
  num_events <- length(events)
  W_sum_total <- numeric(n)
  X_sum_list <- vector('list', num_events)
  comp_list <- vector('list', num_events)
  for (event_index in 1:(num_events)){
    rank_order <- perf %>% 
      filter(event_id == events[event_index]) %>% 
      pull(athlete_id)
    m = length(rank_order)
    index_lst <-match(rank_order, sorted_ids)
    X_sum = Matrix(0, 1, n, sparse=TRUE)
    active_competitors <- Matrix(0, 1, n, sparse=TRUE)
    active_competitors[1,index_lst] <- 1
    W_sum <- active_competitors
    W_sum[1, index_lst[m]] <- 0
    W_sum_total <- W_sum_total + W_sum
    x_vals <- 1:m
    x_vals[m] <- m-1
    X_sum[1, index_lst] <- x_vals
    
    comp_list[[event_index]] <- active_competitors
    X_sum_list[[event_index]] <- X_sum
    
    # print(paste("Writing event", sprintf("%03d",event_index), "out of", num_events))
    # writeMM(active_competitors,
    #           paste(data_loc, "active-competitors-list/list-", sprintf("%03d",event_index), ".txt",sep = ""))
    # writeMM(X_sum,
    #        paste(data_loc, "X-sums/X-sum", sprintf("%03d",event_index), ".txt", sep = ""))
    
    
  }
  list(comp_list,X_sum_list, W_sum_total, events, sorted_ids)
  
  # write(W_sum_total, paste(data_loc, "W-sum.txt", sep = ""))
  # write(events, paste(data_loc, "event-list.txt",sep = ""))
  # write(sorted_ids, paste(data_loc, "population.txt",sep = ""))

}

preprocess_data_test <- function(loc, gender, athlete_information, individual_performance){
  
  # year is string of the desired year
  # loc is string of the location of test-run folder
  # gender is string of either "M" or "F"
  
  # read in data
  #athlete_information <- read_csv(paste(loc,"test-run/data/",year,"/athlete-information-",year,".csv",sep=""))
  #individual_performance <- read_csv(paste(loc,"test-run/data/",year,"/individual-performance-",year,".csv",sep=""), 
  #                                  col_types = cols(pace = col_character(), backup_name =  col_character(), backup_team =  col_character()))
  college_information <- read_csv(paste(loc,"test-run/data/college-team-information.csv",sep=""))
  
  # Select events and population
  college_athletes <- athlete_information %>% 
    filter(team_gender == gender,
           team_name %in% (college_information %>% distinct(team_name) %>% pull()))
  # add filter for athletes on teams!
  perf <- individual_performance %>% 
    filter(!is.na(place), athlete_id %in% (college_athletes %>% pull(athlete_id)))
  # take out filter count > 4
  events <- perf %>% group_by(event_id) %>%
    summarize(count = n()) %>%
    pull(event_id)
  sorted_ids <- perf %>% filter(event_id %in% events) %>%
    distinct(athlete_id) %>% 
    pull(athlete_id) %>% sort()
  n = length(sorted_ids)
  
  # create the W and X matrices for each event result
  # look at paper for clear definitions of W and X
  num_events <- length(events)
  W_sum_total <- numeric(n)
  X_sum_list <- vector('list', num_events)
  comp_list <- vector('list', num_events)
  for (event_index in 1:(num_events)){
    rank_order <- perf %>% 
      filter(event_id == events[event_index]) %>% 
      pull(athlete_id)
    m = length(rank_order)
    index_lst <-match(rank_order, sorted_ids)
    X_sum = Matrix(0, 1, n, sparse=TRUE)
    active_competitors <- Matrix(0, 1, n, sparse=TRUE)
    active_competitors[1,index_lst] <- 1
    W_sum <- active_competitors
    W_sum[1, index_lst[m]] <- 0
    W_sum_total <- W_sum_total + W_sum
    x_vals <- 1:m
    x_vals[m] <- m-1
    X_sum[1, index_lst] <- x_vals
    
    comp_list[[event_index]] <- active_competitors
    X_sum_list[[event_index]] <- X_sum
    
    # print(paste("Writing event", sprintf("%03d",event_index), "out of", num_events))
    # writeMM(active_competitors,
    #           paste(data_loc, "active-competitors-list/list-", sprintf("%03d",event_index), ".txt",sep = ""))
    # writeMM(X_sum,
    #        paste(data_loc, "X-sums/X-sum", sprintf("%03d",event_index), ".txt", sep = ""))
    
    
  }
  
  list(comp_list, X_sum_list, W_sum_total, events, sorted_ids)
  
  # write(W_sum_total, paste(data_loc, "W-sum.txt", sep = ""))
  # write(events, paste(data_loc, "event-list.txt",sep = ""))
  # write(sorted_ids, paste(data_loc, "population.txt",sep = ""))
  
}


