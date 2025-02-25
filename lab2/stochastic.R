func.stochastic <- function(x, y, func){ 
  lower <- c(x[1], y[1])
  upper <- c(x[length(x)], y[length(y)])
  e <- runif(1)
  N <- 5000
  
  z.min <- NULL
  par.res <- list(0, 0)
  for (i in 1:N){
    x.rand <- runif(1:N, lower[1], upper[1])
    y.rand <- runif(1:N, lower[2], upper[2])

    z.curr <- func(c(x.rand, y.rand))
    
    if (i == 1) z.min <- z.curr
    else if (z.min > z.curr){
      z.min <- z.curr
      par.res <- replace(par.res, 1, x.rand)
      par.res <- replace(par.res, 2, y.rand)
    }
  }
  
  print(paste("Function value: ", z.min))
  print(paste("Parameters: ", par.res[1], " and ", par.res[2]))
  print(paste("Number of iterations: ", N))
}