source("lab2/stochastic.R")
source("lab2/annealing.R")

library(optimization)

x.min <- 420.98
y.min <- 420.98
Min.F <- -837.96

x <- y <- seq(-500, 500)
func <- function(par){
  x <- par[1]
  y <- par[2]

  return (-x*sin(sqrt(abs(x))) - y*sin(sqrt(abs(y))))
}

print("")
print("Results after optimisation using Simple Stochastic Search -----------------")
startTime.stoch <- Sys.time()
func.stochastic(x, y, func)
endTime.stoch <- Sys.time()
print(paste("Execution time: ", endTime.stoch-startTime.stoch))

print("")
print("Results after optimisation using Simulated Annealing -----------------")
startTime.anneal <- Sys.time()
func.annealing(x, y, func)
endTime.anneal <- Sys.time()
print(paste("Execution time: ", endTime.anneal-startTime.anneal))

print("")