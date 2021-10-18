require('Matrix')
require(readr)
require(doMC)
suggest_posterior_mean <- function(sigma_vals, prior_mu_vals, n, K_t, X_sum_list, comp_list, W_sum){
  # create the suggested starting vector of posterior means
  pi_0_vals <- Reduce("+", X_sum_list)
  pi_0_vals <- pi_0_vals - W_sum
  m_total <- numeric(n)
  for (k in 1:(K_t)){
    m_total <- m_total + comp_list[[k]] * (sum(comp_list[[k]])-1)
  }
  ((qlogis(0.01 + 0.98*(1-as.matrix(pi_0_vals/m_total))) + 
      prior_mu_vals/sigma_vals^2)/(1+1/sigma_vals^2))
  
}


calc_p_ik<-function(theta_t, X_k_sum){
  # theta_t is
  # X_k is 
  (exp(theta_t)* X_k_sum)/ sum(X_k_sum * exp(theta_t))
}


p_ii_k <- function(p_ii, k){
  p_ik_list[[k]] * (1- p_ik_list[[k]])
}

p_ih_k <- function(k){
  temp <- matrix(p_ik_list[[k]], n, n)
  temp * t(temp)
}

p_val_k <- function(p_vals, k){
  p_ik_list[[k]]
  
}

calc_pi <- function(x){
  p_ih <- matrix(0, n, n)
  end_point <- x + segment_size - 1
  if( end_point > K_t){
    end_point <- K_t
  }
  for (k in x:end_point){
    system(paste("echo",paste("Summing P_ih Race",k, "out of", K_t)))
    # calcs for d2
    temp <- matrix(p_ik_list[[k]], n, n)
    p_ih <- p_ih +  temp * t(temp)
    # calcs for d1
  }
  p_ih
}


#Reduce("+",)

# new one
calculate_derivatives <- function(theta_t, sigma_vals, W_sum, n, K_t, 
                                  prior_mu_vals, p_ik_list){
  
  print("creating p_ih matrix")
  system.time(p_ih <- foreach(i = 1:n, .combine = rbind) %dopar% 
                t((Reduce("+",mapply(function(x) x[i] * x, p_ik_list)))))

  print("Reducing p_ii ")
  system.time(p_ii <- Reduce("+",mapply(function(x) x * (1-x), p_ik_list)))
  print("Reducing p_vals")
  system.time( p_vals <- Reduce("+", p_ik_list))
  print("P_ii")
  p_ii <- -1/sigma_vals^2 - p_ii
  print("returning")
  list((-((theta_t - prior_mu_vals)/sigma_vals^2) + W_sum - p_vals),
    (diag(n)* as.vector(p_ii) + (diag(n) == 0) * p_ih))
  
}

