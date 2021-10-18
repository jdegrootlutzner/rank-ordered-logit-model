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


# generate fake data

res <- data.frame(place = c(1,2,3),
                  athlete_id = c(1,2,3),
                  event_id = rep(1,3))

ath_info <- data.frame(athlete_id = c(1,2,3),
                       team_gender = rep("M",3),
                       team_name = rep("Pomona-Pitzer",3))

repo_loc <- "/Users/Julian/Documents/Fun/"
processed_data_list <- preprocess_data_test(repo_loc, "M", ath_info, res)

comp <- Matrix(c(1,1,1),1,3, sparse = TRUE)
X_sum <- Matrix(c(1,2,2),1,3, sparse = TRUE)
W_sum <- Matrix(c(1,1,0),1,3, sparse = TRUE)
events <- c(1)
sorted_ids <- c(1,2,3)
print("Check preprocessing:")
checkEquals(processed_data_list[[comp_list]][[1]],comp)
checkEquals(processed_data_list[[X_sum_list]][[1]],X_sum)
checkEquals(processed_data_list[[W_sum_total]],W_sum)
checkEquals(processed_data_list[[events_list_index]],events)
checkEquals(processed_data_list[[sorted_ids_index]],sorted_ids)

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

print(as.vector(post_mu_vals))
print(c(qlogis(0.99)/5,qlogis(0.5)/5,qlogis(0.01)/5))
checkEquals(as.vector(post_mu_vals),c(qlogis(0.99)/5,qlogis(0.5)/5,qlogis(0.01)/5))

p_ik_denom<-1*exp(qlogis(0.99)/5) + 2* exp(qlogis(0.5)/5) + 2*exp(qlogis(0.01)/5)
p_ik_check <- c(exp(qlogis(0.99)/5)/p_ik_denom,
                2*exp(qlogis(0.5)/5)/p_ik_denom,
                2*exp(qlogis(0.01)/5)/p_ik_denom)
prev_post_mu_vals <- post_mu_vals


p_ik_list <- foreach(i =1:K_t) %do% 
  (calc_p_ik(post_mu_vals, t(processed_data_list[[X_sum_list]][[i]])))
# with covariance matrix d2

print("p_ik values:")
print("Alg:")
print(as.vector(p_ik_list[[1]]))
print("Tst:")
print(p_ik_check)
checkEquals(as.vector(p_ik_list[[1]]), p_ik_check)

derivative_list <- calculate_derivatives(post_mu_vals, sigma_vals,
                                                           t(processed_data_list[[W_sum_total]]),
                                                           n, K_t, prior_mu_vals,
                                                           p_ik_list)
print("D2 Matrix:")
print("Alg:")
print(derivative_list[[D2]])
print("Tst:")
print(Matrix(c(-4.249248, 0.1781731, 0.07107471, 
               0.1781731, -4.234878, 0.05670457 ,
               0.07107471, 0.05670457 , -4.127779),
             3,3))
checkEquals(round(as.matrix(derivative_list[[D2]]),
                  5),
            round(as.matrix(Matrix(c(-4.249248, 0.1781731, 0.07107471,
                            0.1781731, -4.234878, 0.05670457,
                            0.07107471, 0.05670457, -4.127779),
                            3,3)),
                  5))

print("D1 values")
print("Alg:")
print(as.vector(derivative_list[[D1]]))
print("Tst:")
print(c(-3.14867, 0.6229729,  3.525697))
checkEquals(round(as.vector(derivative_list[[D1]]),5), round(c(-3.14867, 0.6229729, 3.525697),5))

post_mu_vals <- post_mu_vals -
                    solve(derivative_list[[D2]], derivative_list[[D1]])

print("Updated mu_vals:")
print("Alg:")
print(as.vector(post_mu_vals))
print("Tst:")
print(c(0.19750658, 0.12804306, -0.07554958))
checkEquals(round(as.vector(post_mu_vals),5), round(c(0.19750658, 0.12804306, -0.07554958),5))


prev_post_mu_vals <- post_mu_vals
p_ik_list <- foreach(i =1:K_t) %do% 
  (calc_p_ik(post_mu_vals, t(processed_data_list[[X_sum_list]][[i]])))
derivative_list <- calculate_derivatives(post_mu_vals, sigma_vals,
                                         t(processed_data_list[[W_sum_total]]),
                                         n, K_t, prior_mu_vals,
                                         p_ik_list)
post_mu_vals <- post_mu_vals -
  solve(derivative_list[[D2]], derivative_list[[D1]])
print("Second round")
print(as.vector(post_mu_vals))
prev_post_mu_vals <- post_mu_vals
p_ik_list <- foreach(i =1:K_t) %do% 
  (calc_p_ik(post_mu_vals, t(processed_data_list[[X_sum_list]][[i]])))
derivative_list <- calculate_derivatives(post_mu_vals, sigma_vals,
                                         t(processed_data_list[[W_sum_total]]),
                                         n, K_t, prior_mu_vals,
                                         p_ik_list)
post_mu_vals <- post_mu_vals -
  solve(derivative_list[[D2]], derivative_list[[D1]])
print("Third round")
print(as.vector(post_mu_vals))
prev_post_mu_vals <- post_mu_vals
p_ik_list <- foreach(i =1:K_t) %do% 
  (calc_p_ik(post_mu_vals, t(processed_data_list[[X_sum_list]][[i]])))
derivative_list <- calculate_derivatives(post_mu_vals, sigma_vals,
                                         t(processed_data_list[[W_sum_total]]),
                                         n, K_t, prior_mu_vals,
                                         p_ik_list)
post_mu_vals <- post_mu_vals -
  solve(derivative_list[[D2]], derivative_list[[D1]])
print("Fourth round")
print(as.vector(post_mu_vals))
print("Unit Test 1 passed. Rounding allowed to 5th decimal.")
print("_________________________________________________")
print("Starting Unit Test 2:")
res <- data.frame(place = rep(c(1,2,3),2),
                  athlete_id = rep(c(1,2,3),2),
                  event_id = c(c(rep(1,3)),rep(2,3)))

ath_info <- data.frame(athlete_id = c(1,2,3),
                       team_gender = rep("M",3),
                       team_name = rep("Pomona-Pitzer",3))

repo_loc <- "/Users/Julian/Documents/Fun/"
processed_data_list <- preprocess_data_test(repo_loc, "M", ath_info, res)

comp_1 <- Matrix(c(1,1,1),1,3, sparse = TRUE)
comp_2 <- Matrix(c(1,1,1),1,3, sparse = TRUE)
X_sum_1 <- Matrix(c(1,2,2),1,3, sparse = TRUE)
X_sum_2 <- Matrix(c(1,2,2),1,3, sparse = TRUE)
W_sum <- Matrix(c(2,2,0),1,3, sparse = TRUE)
events <- c(1,2)
sorted_ids <- c(1,2,3)
print("Check preprocessing:")
checkEquals(processed_data_list[[comp_list]][[1]],comp_1)
checkEquals(processed_data_list[[comp_list]][[2]],comp_2)
checkEquals(processed_data_list[[X_sum_list]][[1]],X_sum_1)
checkEquals(processed_data_list[[X_sum_list]][[2]],X_sum_2)
checkEquals(processed_data_list[[W_sum_total]],W_sum)
checkEquals(processed_data_list[[events_list_index]],events)
checkEquals(processed_data_list[[sorted_ids_index]],sorted_ids)

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
print(as.vector(post_mu_vals))
print(c(qlogis(0.99)/5,qlogis(0.5)/5,qlogis(0.01)/5))
checkEquals(as.vector(post_mu_vals),c(qlogis(0.99)/5,qlogis(0.5)/5,qlogis(0.01)/5))

p_ik_denom<-1*exp(qlogis(0.99)/5) + 2* exp(qlogis(0.5)/5) + 2*exp(qlogis(0.01)/5)
p_ik_check <- c(exp(qlogis(0.99)/5)/p_ik_denom,
                2*exp(qlogis(0.5)/5)/p_ik_denom,
                2*exp(qlogis(0.01)/5)/p_ik_denom)
prev_post_mu_vals <- post_mu_vals


p_ik_list <- foreach(i =1:K_t) %do% 
  (calc_p_ik(post_mu_vals, t(processed_data_list[[X_sum_list]][[i]])))
# with covariance matrix d2

print("p_ik values:")
print("Alg:")
print(as.vector(p_ik_list[[1]]))
print("Tst:")
print(p_ik_check)
checkEquals(as.vector(p_ik_list[[1]]), p_ik_check)
checkEquals(as.vector(p_ik_list[[2]]), p_ik_check)


derivative_list <- calculate_derivatives(post_mu_vals, sigma_vals,
                                         t(processed_data_list[[W_sum_total]]),
                                         n, K_t, prior_mu_vals,
                                         p_ik_list)
print("D2 Matrix:")
print("Alg:")
print(derivative_list[[D2]])
print("Tst:")
print(Matrix(c(-4.498496, 0.3563462, 0.1421494, 
               0.3563462, -4.469756, 0.1134091 ,
               0.1421494, 0.1134091 , -4.255558),
             3,3))
checkEquals(round(as.matrix(derivative_list[[D2]]),
                  5),
            round(as.matrix(Matrix(c(-4.498496, 0.3563462, 0.1421494,
                                     0.3563462, -4.469756, 0.1134091,
                                     0.1421494, 0.1134091, -4.255558),
                                   3,3)),
                  5))

print("D1 values")
print("Alg:")
print(as.vector(derivative_list[[D1]]))
print("Tst:")
print(c(-2.621244, 1.245946,  3.375298))
checkEquals(round(as.vector(derivative_list[[D1]]),5), round(c(-2.621244, 1.245946, 3.375298),5))

post_mu_vals <- post_mu_vals -
  solve(derivative_list[[D2]], derivative_list[[D1]])

print("Updated mu_vals:")
print("Alg:")
print(as.vector(post_mu_vals))
print("Tst:")
print(c(0.3812983, 0.2557222, -0.1370204))
checkEquals(round(as.vector(post_mu_vals),5), round(c(0.3812983, 0.2557222, -0.1370204),5))


prev_post_mu_vals <- post_mu_vals
p_ik_list <- foreach(i =1:K_t) %do% 
  (calc_p_ik(post_mu_vals, t(processed_data_list[[X_sum_list]][[i]])))
derivative_list <- calculate_derivatives(post_mu_vals, sigma_vals,
                                         t(processed_data_list[[W_sum_total]]),
                                         n, K_t, prior_mu_vals,
                                         p_ik_list)
post_mu_vals <- post_mu_vals -
  solve(derivative_list[[D2]], derivative_list[[D1]])
print("Second round")
print(as.vector(post_mu_vals))
prev_post_mu_vals <- post_mu_vals
p_ik_list <- foreach(i =1:K_t) %do% 
  (calc_p_ik(post_mu_vals, t(processed_data_list[[X_sum_list]][[i]])))
derivative_list <- calculate_derivatives(post_mu_vals, sigma_vals,
                                         t(processed_data_list[[W_sum_total]]),
                                         n, K_t, prior_mu_vals,
                                         p_ik_list)
post_mu_vals <- post_mu_vals -
  solve(derivative_list[[D2]], derivative_list[[D1]])
print("Third round")
print(as.vector(post_mu_vals))
prev_post_mu_vals <- post_mu_vals
p_ik_list <- foreach(i =1:K_t) %do% 
  (calc_p_ik(post_mu_vals, t(processed_data_list[[X_sum_list]][[i]])))
derivative_list <- calculate_derivatives(post_mu_vals, sigma_vals,
                                         t(processed_data_list[[W_sum_total]]),
                                         n, K_t, prior_mu_vals,
                                         p_ik_list)
post_mu_vals <- post_mu_vals -
  solve(derivative_list[[D2]], derivative_list[[D1]])
print("Fourth round")
print(as.vector(post_mu_vals))
print("Unit test 2 passed. Rounding allowed to 5th decimal.")
  