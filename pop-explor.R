library(readr)
library(dplyr)
library(ggplot2)
source("~/Documents/Fun/test-run/exploration/rank-ordered-logit-model-for-strength-ratings/preprocess-data-optimize.R")
source("~/Documents/Fun/test-run/exploration/rank-ordered-logit-model-for-strength-ratings/newton-raphson-optimize.R")

sigma <- 0.5

comp_list <- 1
X_sum_list <- 2 
W_sum_total <-3
events <- 4
sorted_ids <- 5
loc <- "~/Documents/Fun/"
gender <- "M"
year <- 2019
athlete_information <- read_csv(paste(loc,"test-run/data/",year,"/athlete-information-",year,".csv",sep=""))
team_information <- read_csv(paste(loc,"test-run/data/college-team-information.csv",sep=""))
individual_performance <- read_csv(paste(loc,"test-run/data/",year,"/individual-performance-",year,".csv",sep=""), 
                                   col_types = cols(pace = col_character(), backup_name =  col_character(), backup_team =  col_character()))
college_information <- read_csv(paste(loc,"test-run/data/college-team-information.csv",sep=""))

#theta <- scan("~/Desktop/prelim-results/M/theta-831.txt")
theta <- scan("~/Desktop/resu/M/theta-001.txt")



sorted_ids <-  scan("~/Desktop/resu/M/population.txt")
events <-  scan("~/Desktop/resu/M/event-list.txt")
res <- data.frame(theta, athlete_id = sorted_ids)

#ggplot(res) + geom_histogram(aes(x = theta), bins= 30)
res <- individual_performance %>% 
  filter(!is.na(place),
         athlete_id %in% sorted_ids) %>% 
  group_by(athlete_id) %>% 
  summarise(race_count = n()) %>%
  inner_join(res, by = "athlete_id")


process_loc <- paste(loc, "test-run/data/", year, "/", gender, "/", sep = "")
#res <- res %>% inner_join(athlete_information, by = "athlete_id")
processed_data_list <- preprocess_data(year, loc, gender)
K_t <- length(processed_data_list[[events]])
n <-  length(processed_data_list[[sorted_ids]])
prior_mu_vals <- numeric(n)
sigma_vals <- rep(sigma,n)
post_mu_vals <- suggest_posterior_mean(sigma_vals, prior_mu_vals, n, K_t,
                                       processed_data_list[[X_sum_list]],
                                       processed_data_list[[comp_list]],
                                       processed_data_list[[W_sum_total]])
post_mu_vals <- t(post_mu_vals)
res <-data.frame(res, post_mu_vals)

ggplot(res,aes(x = race_count)) + geom_point(aes(y=post_mu_vals, color="post_mu_vals")) + 
  geom_point(aes(y=theta, color = "theta"))  +
  theme(legend.position = "bottom") + labs(y= "Strength Values", x="Number of Races")


ggplot(res) + geom_density(aes(theta, color="theta")) + 
  geom_density(aes(post_mu_vals, color ="post_mu_vals")) + 
  theme(legend.position = "bottom")

## WEST ANALYSIS
theta_5 <- scan("~/Desktop/west/theta-sigma-5.txt")

theta_2 <- scan("~/Desktop/west/theta-sigma-2.txt")
theta_1 <- scan("~/Desktop/west/theta-sigma-1.txt")
theta_0.5 <- scan("~/Desktop/west/theta-sigma-0.5.txt")
theta_0.3 <- scan("~/Desktop/west/theta-sigma-0.3.txt")
theta_0.1 <- scan("~/Desktop/west/theta-sigma-0.1.txt")

west_ids <- scan("~/Desktop/west/population.txt")
west_events <- scan("~/Desktop/west/event-list.txt")
west_res <- data.frame(theta_5, theta_2, theta_1, theta_0.5, theta_0.3, theta_0.1, athlete_id = west_ids)

west_res <- individual_performance %>% 
  filter(!is.na(place),
         athlete_id %in% west_ids) %>% 
  group_by(athlete_id) %>% 
  summarise(race_count = n()) %>%
  inner_join(west_res, by = "athlete_id")

west_res <- west_res %>% inner_join(athlete_information, by="athlete_id")
west_res %>% View()

## POMONA 
pom_0.3 <- scan("~/Desktop/pomona/theta-sigma-0.3.txt")
pom_0.4 <- scan("~/Desktop/pomona/theta-sigma-0.4.txt")
pom_0.5 <- scan("~/Desktop/pomona/theta-sigma-0.5.txt")
pom_0.8 <- scan("~/Desktop/pomona/theta-sigma-0.8.txt")

pom_1 <- scan("~/Desktop/pomona/theta-sigma-1.txt")
pom_2 <-scan("~/Desktop/pomona/theta-sigma-2.txt")
pom_5 <-scan("~/Desktop/pomona/theta-sigma-5.txt")

pom_ids <- scan("~/Desktop/pomona/population.txt")
pom_events <- scan("~/Desktop/pomona/event-list.txt")


pom_sigma_0.4 <- scan("~/Desktop/pomona/theta-sigma-sigma-0.4.txt")
pom_sigma_0.8 <- scan("~/Desktop/pomona/theta-sigma-sigma-0.8.txt")
pom_sigma_2 <- scan("~/Desktop/pomona/theta-sigma-sigma-2.txt")
pom_sigma_5 <- scan("~/Desktop/pomona/theta-sigma-sigma-5.txt")

pom_res <- data.frame(pom_2,pom_sigma_2, pom_5,pom_sigma_5 , athlete_id = pom_ids)
pom_res <- individual_performance %>% 
  filter(!is.na(place),
         athlete_id %in% pom_ids) %>% 
  group_by(athlete_id) %>% 
  summarise(race_count = n()) %>%
  inner_join(pom_res, by = "athlete_id")

pom_res <- pom_res %>% inner_join(athlete_information, by="athlete_id")
pom_res %>% View()



