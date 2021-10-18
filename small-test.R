library(readr)
library(dplyr)
library('Matrix')
#library(VGAM)

# @author Julian DeGroot-Lutzner
# @date Spring 2020

# Before applying the algorithm on the complete dataset I coded it out and tested
# it on a smaller datset to see that I knew how it works.
# 
# I worked on a dataset of results from the Pomona-Pitzer mens 2019 xc team because
# I am familiar with all the athletes and their skill levels.
# 
# The test was successful. The predicted scores seem reasonable for the test dataset.
# and I was able to parralelize the results.
# 
# This process also helped me think about how to best code this for the complete
# dataset.
# > All functions were vectorized for efficiency
# > I have a feeling this code may run faster in Matlab because I will be working
# with large matrices, and will have to find the inverse of large matrices. 
# > I can store all of the race matrices rolled up in a one file on top of eachother
# with [row saying dimensions] on top of [X matrix] on top of [ W_matrix] . Repeat

# read in 2019 data 
athlete_information <- read_csv("~/Documents/Fun/test-run/data/2019/athlete-information-2019.csv")
team_information <- read_csv("~/Documents/Fun/test-run/data/college-team-information.csv")
individual_performance <- read_csv("~/Documents/Fun/test-run/data/2019/individual-performance-2019.csv", col_types = cols(pace = col_character(), backup_name =  col_character(), backup_team =  col_character()))

# create subset of data for Pomona-Pitzer men
college_athletes <- athlete_information %>% 
  filter(team_gender == 'M', team_name == 'Pomona-Pitzer')
perf <- individual_performance %>% 
  filter(!is.na(place), athlete_id %in% (college_athletes %>% pull(athlete_id)))
sorted_ids <- perf %>% distinct(athlete_id) %>% 
  pull(athlete_id) %>% sort()
event_count <- perf %>% group_by(event_id) %>% summarize(count = n())
events <- event_count %>% pull(event_id)
n = length(sorted_ids)

# create the W and X matrices for each event result
# look at paper for clear definitions of W and X
comp_list = list()
X_list = list()
W_list = list()
num_events = length(events)
for (event_index in 1:(num_events)){
  print(paste('Processing event:',event_index,'/',num_events))
  rank_order <- perf %>% 
    filter(event_id == events[event_index]) %>% 
    pull(athlete_id)
  m = length(rank_order)
  index_lst <-match(rank_order, sorted_ids)
  X = Matrix(0, m-1, n, sparse=TRUE)
  W = Matrix(0, m-1, n, sparse=TRUE)
  active_competitors <- numeric(n)
  for (i in 1:(m-1)){
    index <-index_lst[i]
    W[i,index] = 1
    active_competitors[index] = 1
    for (j in i:(m)){
      if(j != i){
        X[i,index_lst[j]] = 1 
      }
    }
  }
  #save matrices for each event
  X_list[[event_index]] = X
  W_list[[event_index]] = W
  active_competitors[index_lst[m]]=1
  comp_list[[event_index]] = active_competitors
  
}

# set prior mu vals and sigma vals
prior_mu_vals <- numeric(n)
sigma <-.1
sigma_vals <- rep(sigma,n)

# create the suggested starting vector of posterior means
pi_0_vals <- numeric(n)
m_total <- numeric(n)

for (i in 1:(num_events)){
  pi_0_vals <- pi_0_vals + colSums(X_list[[i]])
  m_total <- m_total + comp_list[[i]] * (sum(comp_list[[i]])-1)
}
pi_0_vals <- pi_0_vals/m_total
q_0_vals <-qlogis(0.01 + 0.98*(1-pi_0_vals))
post_mu_vals <- (q_0_vals + prior_mu_vals/sigma_vals^2)/(1+1/sigma_vals^2)


# initialize functions for calculating different parts of 
# the Newton-Raphson algorithm


calc_p_ik<-function(theta_t, k){
    return((exp(theta_t)* colSums(X_list[[k]]))/
             sum(X_list[[k]] %*% exp(theta_t)))
  }

create_W_sum <- function(K_t, n){
  # Even though calc_D1 asks for this value every time, we only need 
  # to calculate this once because the value is independent of theta
  W_sum <- numeric(n)
  for (k in (1:K_t)){
    print(paste('Creating W_sum for event:',k,'/',K_t))
    W_sum <- W_sum + colSums(W_list[[k]])
  }
  return(W_sum)
}

calc_D1 <-function( theta_t, n, K_t, W_sum){
  p_vals <- numeric(n)
  for (k in (1:K_t)){
    p_vals <- p_vals + calc_p_ik(theta_t, k)
  }
  return(-((theta_t - prior_mu_vals)/sigma_vals^2) + W_sum - p_vals)
}

calc_D2 <-function(theta_t, n, K_t){
  
  p_ii <- numeric(n)
  p_ih <- matrix(0,n,n)
  for (k in 1:K_t){
    p_ii <- p_ii + calc_p_ik(theta_t, k) * (1- calc_p_ik(theta_t, k))
    temp <- matrix(calc_p_ik(theta_t, k), n, n)
    p_ih <- p_ih +  temp * t(temp)
  }
  p_ii <- -1/sigma_vals^2 - p_ii
  return(diag(n)* as.vector(p_ii) + (diag(n) == 0) * p_ih)
  
}

# initialize W value for calc_D1
W_sum <- create_W_sum(num_events, n)

# for testing you could use a hardset number of iterations
# but a more robust implementation using error vals is implemented 

# for (j in 1:10){
#   prev_post_mu_vals <- post_mu_vals
#   post_mu_vals <- post_mu_vals - 
#     (solve(calc_D2(post_mu_vals, n, num_events)) %*% 
#        calc_D1(post_mu_vals, n, num_events, W_sum))
#   print(as.vector(post_mu_vals)[1:9])
#   
# }

# repeat process until there is a neglible change in values
e <- Inf
epsilon <- 0.0000001
while (e > epsilon){
  prev_post_mu_vals <- post_mu_vals
  

  
  post_mu_vals <- post_mu_vals - solve(calc_D2(post_mu_vals, n, num_events), 
  calc_D1(post_mu_vals, n, num_events, W_sum))
  
  print(as.vector(post_mu_vals)[1:9])
  e <- sum((prev_post_mu_vals- post_mu_vals)^2)
  print(e)
  
}
updated_sigma_vals <- sqrt(-diag(solve(calc_D2(post_mu_vals, n, num_events))))
print(updated_sigma_vals[1:7])
#print(sqrt(-solve(calc_D2(post_mu_vals,n,num_events),diag(n)))[1:7])
# combine the strength scores with the athletes and View them to see if they make sense
res <- data.frame(athlete_id = sorted_ids, post_mu_vals)
res <- inner_join(res, college_athletes, by = 'athlete_id')

res <- res %>% inner_join(
  data.frame(athlete_id = sorted_ids, num_races = Reduce("+", comp_list)),
  by = "athlete_id")

res %>% arrange(post_mu_vals) %>% View()
# Code I used in figuring out what was happenning in the paper
# keeping here for now

# library(ggplot2)
# 
# plot_gumbel <- function(loc = 0){
#   x= seq(-3 + loc, 5 + loc,0.1)
#   y= dgumbel(seq(-3+loc,5+loc,0.1), location = loc)
#   return(geom_point(data = data.frame(x,y), aes(x=x,y=y)) )
# }
# ggplot() + plot_gumbel(0)  + plot_gumbel(2)
# # How does a 1 point difference in theta describe probability of beating competitor
# ?resmu = numeric(n)
# sigma = numeric(n)
# theta = numeric(n)
