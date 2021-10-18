
# load master athletet list 
# athlete id, name, school, gender, 2012.races_finished, 2012.theta, 2012.mu, 2012.sigma,


source("double-check-preprocess.R")
source("newton-raphson-optimize.R")
require(foreach)
# init. values 


# tuning variables
epsilon <- 0.0000001 # used 
sigma <- 0.5
# data loc
repo_loc <- "~/Documents/Fun/"
data_loc <- "test-run/d/"
gender <- "M"

# improve readability for indices of processed data list
comp_list <- 1
X_sum_list <- 2 
W_sum_total <-3
events <- 4
sorted_ids <- 5
D1 <-1 
D2 <-2 

# paralleization
ncores <- detectCores(logical = FALSE)
print(paste("Number of cores:", ncores))
registerDoMC(ncores)


# Recursively loop over each year
for (year in 2019:2019){
  # loc <- paste(repo_loc, data_loc, year, "/", gender, "/", sep = "")
  
  # preprocess data
  print("Preprocessing data.")
  processed_data_list <- preprocess_data(year, repo_loc, gender)
  
  print(paste("Done. Processed data size:",object.size(processed_data_list)))
  K_t <- length(processed_data_list[[events]])
  n <-  length(processed_data_list[[sorted_ids]])
  prior_mu_vals <- numeric(n)
  sigma_vals <- rep(sigma,n)
  
  print("Saving event list and population")
  # write(processed_data_list[[events]], paste(loc, "event-list.txt",sep = ""))
  # write(processed_data_list[[sorted_ids]], paste(loc, "population.txt",sep = ""))
  
  # Calculate novel theta_0, mu_0 -----------
  
  print("Creating suggested theta_0.")
  post_mu_vals <- suggest_posterior_mean(sigma_vals, prior_mu_vals, n, K_t,
                                         processed_data_list[[X_sum_list]],
                                         processed_data_list[[comp_list]])
  post_mu_vals <- t(post_mu_vals)
  
  print(as.vector(post_mu_vals)[1:7])
  
  # Prior norm. = norm_(t-1) theta_0, mu_0 -----------
  
  # Prior dist. assumed to consist of ind. normal distrib. comp. -----------
  
  # Game data observed, aprox. norm. post. dist. computed -----------
  
  
  # calculate W_sum
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
    system.time(derivative_list <- calculate_derivatives(post_mu_vals, sigma_vals,
                                                         t(processed_data_list[[W_sum_total]]),
                                                         n, K_t, prior_mu_vals,
                                                         p_ik_list))
    print("Solving and updaing mu vals")
    print(system.time(post_mu_vals <- post_mu_vals -
                        solve(derivative_list[[D2]], derivative_list[[D1]])))
    
    print("Writing updated mu vals to file. First 9 mu_vals:")
    print(as.vector(post_mu_vals)[1:7])
    #write(as.vector(post_mu_vals),
    #      paste(loc, "theta-",sprintf("%03d",iter),".txt",sep = ""))
    e <- sum((prev_post_mu_vals- post_mu_vals)^2)
    #write(e,
    #      paste(loc, "e-",sprintf("%03d",iter),".txt",sep = ""))
    print(paste("Error:", e))
    iter <- iter + 1
    
  }
  print('Writing covariance matrix')
  #write(as.matrix(derivative_list[[D2]]), 
  #      paste(loc, "cv-matrix.txt",sep = ""))
  
  # Obtain prior norm. for next rating period using inn. matrix -----------
  
  
}
