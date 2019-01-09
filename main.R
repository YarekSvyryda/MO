rm(list=ls())

# Set WD
#setwd('C:/Users/dkaszy/Desktop/NMO2/')

# Load functions
source('numGradient.R')
source('gradientDescent.R')
source('steepestDescent.R')

# Objective function
myFun = function(x){
  return(x[1]^2 + exp(x[1])*1/10*x[2]^2)
}

# Set Params
xSeed   <- c(2,4)
n_grid  <- 200

# Run algorithms
gd1 <- gradientDescent(myFun, xSeed, 0.01, 10^-10, 1000)
sd1 <- steepestDescent(myFun, xSeed, 1, 10^-10, 1000)

# Plot convergence of f 
plot(1:length(gd1$f_hist), gd1$f_hist, type='l', col='blue')
lines(1:length(sd1$f_hist), sd1$f_hist, type='l', col='red')

# Plot Evaluation time
plot(1:length(gd1$tEval), gd1$tEval, col='blue')
lines(1:length(sd1$tEval), sd1$tEval, col='red')

# Plot optimization trajectories
x_seq <- seq(-5, 5, length = n_grid)
matrVal <- matrix(0, nrow = n_grid, ncol = n_grid)
for(iRow in 1 : n_grid){
  for(iCol in 1 : n_grid){
    matrVal[iRow, iCol] <- myFun(c(x_seq[iRow], x_seq[iCol]))    
  }
}
contour(x_seq, x_seq, matrVal, nlevels = 400)
lines(gd1$x_hist, col = 'blue', type = 'l')
lines(sd1$x_hist, col = 'red', type = 'l')