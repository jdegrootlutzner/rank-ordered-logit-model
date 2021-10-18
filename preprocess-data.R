
require(readr)
require(dplyr)
require('Matrix')


# function to create list of matrices 
create_matrix_dataset <- function(year, loc, gender){
  processed_data_loc <- "test-run/exploration/rank-ordered-logit-model-for-strength-ratings/processed-data/"
  data_loc <- paste(loc, processed_data_loc, year, "/" , gender, "/", sep = "")
  
  # year is string of the desired year
  # loc is string of the location of test-run folder
  # gender is string of either "M" or "F"
  
  # read in data
  athlete_information <- read_csv(paste(loc,"test-run/data/",year,"/athlete-information-",year,".csv",sep=""))
  team_information <- read_csv(paste(loc,"test-run/data/college-team-information.csv",sep=""))
  individual_performance <- read_csv(paste(loc,"test-run/data/",year,"/individual-performance-",year,".csv",sep=""), 
                                     col_types = cols(pace = col_character(), backup_name =  col_character(), backup_team =  col_character()))
  college_information <- read_csv(paste(loc,"test-run/data/college-team-information.csv",sep=""))
  
  # Select events and population
  college_athletes <- athlete_information %>% 
    filter(team_gender == gender,
           team_name %in% (college_information %>% distinct(team_name) %>% pull()))
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
  num_events = length(events)
  for (event_index in 1:(num_events)){
    rank_order <- perf %>% 
      filter(event_id == events[event_index]) %>% 
      pull(athlete_id)
    m = length(rank_order)
    index_lst <-match(rank_order, sorted_ids)
    X = Matrix(0, m-1, n, sparse=TRUE)
    W = Matrix(0, m-1, n, sparse=TRUE)
    active_competitors <- Matrix(0, 1, n, sparse=TRUE)
    for (i in 1:(m-1)){
      index <-index_lst[i]
      W[i,index] = 1
      active_competitors[1,index] = 1
      for (j in i:(m)){
        if(j != i){
          X[i,index_lst[j]] = 1
        }
      }
    }
    # add last competitor
    active_competitors[index_lst[m]]=1
    
    print(paste("Writing event", sprintf("%03d",event_index), "out of", num_events))
    writeMM(active_competitors,
              paste(data_loc, "active-competitors-list/list-", sprintf("%03d",event_index), ".txt",sep = ""))
    writeMM(X,
           paste(data_loc, "X-matrices/X-", sprintf("%03d",event_index), ".txt", sep = ""))
    writeMM(W,
           paste(data_loc, "W-matrices/W-", sprintf("%03d",event_index),".txt", sep = ""))
    
  }
  print("Finishing up")
  write(events, paste(data_loc, "event-list.txt",sep = ""))
  write(sorted_ids, paste(data_loc, "population.txt",sep = ""))

}
year <- 2019
loc <- "~/Documents/Fun/"
gender <- "F"
create_matrix_dataset(year,loc,gender)

#writeMM(C,"~/Desktop/test")
#readMM("~/Desktop/test.txt")
# need to get n for next part . n = num of athletes in pop.

