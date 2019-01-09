steepestDescent = function(f, x, a, e, maxIter){
  # steepestDescent
  # INPUT
  #      - f objective function
  #      - x inital coordinates
  #      - a step size
  #      - e termination criterion 
  #      - maxIter maximum number of iterations
  
  result <- list(x_opt = x, f_opt = f(x), x_hist = x, f_hist = f(x), tEval = 0, iter = 0)
  
  currIter <- 0
  finished <- FALSE
  x_old <- x
  while(finished == FALSE){
    StartT <- Sys.time()
    x_new <- lineSearch(f, x_old, x_old - a*numGradient(f, x_old, 10^-6), 1000)
    #x_new <- x_old - a*numGradient(f, x_old, 10^-6)
    if(currIter <= maxIter & abs(f(x_new)-f(x_old))>e & f(x_new)<f(x_old)){
      x_old <- x_new
      result$x_opt  <- x_new
      result$f_opt  <- f(x_new)
      result$x_hist <- rbind(result$x_hist, x_new)
      result$f_hist <- rbind(result$f_hist, f(x_new))
      result$iter   <- currIter
      result$tEval   <- rbind(result$tEval, Sys.time() - StartT)
    }else{
      finished <- TRUE
    }
    currIter <- currIter + 1
  }
  return(result)
}

lineSearch = function(f, x0, x1, gridSize){
  # lineSearch
  # INPUT
  #      - f objective function
  #      - x0 starting point
  #      - x1 new point (in terms of gradient descent algorithm)
  #      - gridSize number of points between x0 and x1 
  
  x_best <- x0
  for(i in 1 : gridSize){
    t <- i/gridSize
    x_new <- t*x1 + (1-t)*x0
    if(f(x_best)>f(x_new)){
      x_best <- x_new
    }
  }
  return(x_best)
}

