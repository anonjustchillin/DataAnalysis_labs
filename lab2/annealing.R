library(optimization)


func.annealing <- function(x, y, func){
  lower <- c(x[1], y[1])
  upper <- c(x[length(x)], y[length(y)])

  func.sa <- optim_sa(fun=func,
                      start=c(0,0),
                      lower=lower,
                      upper=upper)
  print(func.sa)
}
  
  