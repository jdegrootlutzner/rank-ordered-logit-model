require(RUnit)

source("preprocess-data-optimize.R")
source("newton-raphson-optimize.R")
require(Matrix)
comp_list <- 1
X_sum_list <- 2 
W_sum_total <-3
events_list_index <- 4
sorted_ids_index <- 5
D1 <-1 
D2 <-2 

epsilon <- 0.001
# generate fake data

make_fake_win <-function(start,end){
  c<-c()
  for (i in start:end){
    c <- c(c,c(1,i))
  }
  c
}
make_fake_lose <-function(start, end){
  c<-c()
  for (i in start:end){
    c <- c(c,c(i,1))
  }
  c
}
ath<-2
res <- data.frame(place = rep(c(1,2),2*ath),
                  athlete_id = c(make_fake_win(2,ath+1), make_fake_lose(ath+2, 2*ath+1)),
                  event_id = rep(1:(2*ath), each=2))

ath_info <- data.frame(athlete_id = 1:(2*ath+1),
                       team_gender = rep("M",(2*ath+1)),
                       team_name = rep("Pomona-Pitzer",(2*ath+1)))
repo_loc <- "/Users/Julian/Documents/Fun/"
processed_data_list <- preprocess_data_test(repo_loc, "M", ath_info, res)
sigma <- 0.5
K_t <- length(processed_data_list[[events_list_index]])
n <-  length(processed_data_list[[sorted_ids_index]])
prior_mu_vals <- numeric(n)
sigma_vals <- rep(sigma,n)
post_mu_vals <- suggest_posterior_mean(sigma_vals, prior_mu_vals, n, K_t,
                                       processed_data_list[[X_sum_list]],
                                       processed_data_list[[comp_list]],
                                       processed_data_list[[W_sum_total]])
post_mu_vals <- t(post_mu_vals)


e <- Inf
iter <- 1
print("Training model:")

while (e > epsilon){
  print(paste("Iteration:", iter))
  
  prev_post_mu_vals <- post_mu_vals
  
  #calculate p_ik for each event and save to a matrix 
  # p_ik is dim 1 x n
  # matrix will be K x n, or list will be K long of 1xn
  print("Creating p_ik_list")
  
  p_ik_list <- foreach(i =1:K_t) %do% 
    (calc_p_ik(post_mu_vals, t(processed_data_list[[X_sum_list]][[i]])))
  # with covariance matrix d2
  
  print("Calculating derivatives")
  print(system.time(derivative_list <- calculate_derivatives(post_mu_vals, sigma_vals,
                                                             t(processed_data_list[[W_sum_total]]),
                                                             n, K_t, prior_mu_vals,
                                                             p_ik_list)))
  print("Solving and updaing mu vals")
  print(system.time(post_mu_vals <- post_mu_vals -
                      solve(derivative_list[[D2]], derivative_list[[D1]])))
  
  print("Writing updated mu vals to file. First 9 mu_vals:")
  print(as.vector(post_mu_vals)[1:7])

  e <- sum((prev_post_mu_vals- post_mu_vals)^2)
  print(paste("Error:", e))
  iter <- iter + 1
  
}

