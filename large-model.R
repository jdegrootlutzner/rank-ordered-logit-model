
# load master athletet list 
# athlete id, name, school, gender, 2012.races_finished, 2012.theta, 2012.mu, 2012.sigma,

library(lineprof)

source("newton-raphson.R")
# init. values 


# tuning variables
epsilon <- 0.0000001 # used 
sigma <- 0.5
# data loc
repo_loc <- "~/Documents/Fun/"
data_loc <- "test-run/exploration/rank-ordered-logit-model-for-strength-ratings/processed-data/"
gender <- "M"


# Recursively loop over each year
for (year in 2019:2019){
  loc <- paste(repo_loc, data_loc, year, "/", gender, "/", sep = "")
 
  K_t <- length(scan(paste(loc,"event-list.txt", sep = "")))
  n <- length(scan(paste(loc,"population.txt", sep = "")))
  prior_mu_vals <- numeric(n)
  sigma_vals <- rep(sigma,n)
  
  # Calculate novel theta_0, mu_0 -----------
  
  print("Creating suggested theta_0.")
  post_mu_vals <- suggest_posterior_mean(sigma_vals, prior_mu_vals, n, K_t,
                                         paste(loc, "X-matrices/",sep = ""),
                                         paste(loc, "active-competitors-list/",sep = ""))
  
  post_mu_vals <- t(post_mu_vals)
  # Prior norm. = norm_(t-1) theta_0, mu_0 -----------
  
  
  # Prior dist. assumed to consist of ind. normal distrib. comp. -----------
  
  
  
  
  # Game data observed, aprox. norm. post. dist. computed -----------
  
  
  # calculate W_sum
  print("Creating W_sum object.")
  W_sum <-create_W_sum(
    length(scan(paste(loc,"event-list.txt", sep = ""))),
    length(scan(paste(loc,"population.txt", sep = ""))),
    paste(loc, "W-matrices/", sep = ""))

  e <- Inf
  i <- 1
  print("Training model:")
  while (e > epsilon){
    print(paste("Iteration:", i))
    
    prev_post_mu_vals <- post_mu_vals
    post_mu_vals <- post_mu_vals -
      (solve(calc_D2(post_mu_vals, sigma_vals, n, K_t,
                     paste(loc,"X-matrices/", sep = ""))) %*%
         calc_D1(post_mu_vals, sigma_vals, W_sum,
                 n, K_t, prior_mu_vals,
                 paste(loc,"X-matrices/", sep = "")))
    print(as.vector(post_mu_vals)[1:9])
    write(post_mu_vals,
          paste(loc, "theta-vals/theta-",sprintf("%03d",i),".txt",sep = ""))
    e <- sum((prev_post_mu_vals- post_mu_vals)^2)
    write(e,
          paste(loc, "theta-vals/e-",sprintf("%03d",i),".txt",sep = ""))
    print(paste("Error:", e))
    i <- i + 1

  }
  
  # Obtain prior norm. for next rating period using inn. matrix -----------
  
  
  
  
}
