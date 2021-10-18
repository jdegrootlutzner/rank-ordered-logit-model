
require(parallel)

require('Matrix')


foo <- function(x, segment_size, num_events){
  end_point <- x + segment_size
  if(end_point > num_events){
    end_point <- num_events
  }
  sum(x:end_point)
}
#, segment_size, K_t, p_ik_list, n
calc_pi <- function(x){
  p_ih <- matrix(0,n,n)
  end_point <- x + segment_size-1
  if( end_point > K_t){
    end_point <- K_t
  }
  for (k in x:end_point){
    print(paste("Summing P_ih Race",k, "out of", K_t))
    # calcs for d2
    temp <- matrix(p_ik_list[[k]], n, n)
    p_ih <- p_ih +  temp * t(temp)
    # calcs for d1
  }
  p_ih
}

calc <- function(){
  p_ik_list <- list(Matrix(1:5,1,5),Matrix(6:10, 1,5),
                    Matrix(11:15, 1,5), Matrix(16:20, 1,5))
  K_t <- 4
  n <- 5
  
  print('a')
  ncores <- detectCores(logical = FALSE)
  segment_size <- ceiling(K_t/ncores)


  #setup parallel backend to use many processors
  cl <- makeCluster(ncores)
  
  clusterEvalQ(cl, library('Matrix'))
  clusterExport(cl, list("p_ik_list","K_t","n","segment_size"),
                envir=environment())
  part_sums <- clusterApply(cl,
                            fun = calc_pi,
                            x = seq(from =1, to = K_t, by = segment_size ))
  total <- Reduce("+",part_sums)
  stopCluster(cl)
  total
}

print(calc())
#stop cluster


# x <- list(Matrix(1:5,5,1),Matrix(6:10,5,1))
# lapply(seq_along(x),function(i)
#   unlist(x[i]))
