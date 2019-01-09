numGradient = function(f, x, h){
  # numGradient
  # INPUT
  #      - f objective function
  #      - x coordinates
  #      - h numerical perturbation

  n <- length(x)
  g <- rep(0, n)
  for(i in 1 : n){
   e <- rep(0, n)
   e[i] <- 1
   g[i] <- (f(x+e*h)-f(x-e*h))/(2*h)
  }
  return(g)
}