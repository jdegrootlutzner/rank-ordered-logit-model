require('Matrix')
require(readr)

suggest_posterior_mean <- function(sigma_vals, prior_mu_vals, n, K_t, X_matrices_loc, comp_list_loc){
  # create the suggested starting vector of posterior means
  pi_0_vals <- numeric(n)
  m_total <- numeric(n)
  for (k in 1:(K_t)){
    pi_0_vals <- pi_0_vals + colSums(
      readMM(paste(X_matrices_loc, "X-", sprintf("%03d", k),".txt",sep ="")))
    comp_list <- readMM(paste(comp_list_loc, "list-", sprintf("%03d", k),".txt",sep =""))    
    m_total <- m_total + comp_list * (sum(comp_list)-1)
  }
  return((qlogis(0.01 + 0.98*(1-as.matrix(pi_0_vals/m_total))) + 
           prior_mu_vals/sigma_vals^2)/(1+1/sigma_vals^2))
  
}


calc_p_ik<-function(theta_t, X_k){
  # theta_t is
  # X_k is 
  return((exp(theta_t)* colSums(X_k))/
           sum(X_k %*% exp(theta_t)))
}

create_W_sum<- function(K_t, n, K_matrices_loc){
  # Even though calc_D1 asks for this value every time, we only need 
  # to calculate this once because the value is independent of theta
  
  # this could be done once and saved to file
  W_sum <- numeric(n)
  for (k in (1:K_t)){
    W_sum <- W_sum + 
      colSums(
        readMM(paste(K_matrices_loc, "W-", sprintf("%03d", k),".txt",sep ="")))
  }
  return(W_sum)
}

calc_D1 <-function( theta_t, sigma_vals, W_sum, n, K_t, 
                    prior_mu_vals, X_matrices_loc){
  p_vals <- numeric(n)
  for (k in (1:K_t)){
    print(paste("calc_D1. Event",k, "out of", K_t))
    p_vals <- p_vals + 
      calc_p_ik(theta_t, 
                readMM(paste(X_matrices_loc, "X-", sprintf("%03d", k),".txt",sep ="")))
  }
  return(-((theta_t - prior_mu_vals)/sigma_vals^2) + W_sum - p_vals)
}

calc_D2 <-function(theta_t, sigma_vals, n, K_t, X_matrices_loc){
  
  p_ii <- numeric(n)
  p_ih <- matrix(0,n,n)
  for (k in 1:K_t){
    print(paste("calc_D2. Event",k, "out of", K_t))
    X_k <- readMM(paste(X_matrices_loc, "X-", sprintf("%03d", k),".txt",sep =""))
    p_ii <- p_ii + calc_p_ik(theta_t, X_k) * (1- calc_p_ik(theta_t, X_k))
    temp <- matrix(calc_p_ik(theta_t, X_k), n, n)
    p_ih <- p_ih +  temp * t(temp)
  }
  p_ii <- -1/sigma_vals^2 - p_ii
  return(diag(n)* as.vector(p_ii) + (diag(n) == 0) * p_ih)
  
}