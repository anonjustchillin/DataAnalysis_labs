library(optimization)


func.annealing <- function(x, y, func){
  # межі
  lower <- c(x[1], y[1])
  upper <- c(x[length(x)], y[length(y)])
  
  # виклик функції optim_sa,
  # яка приймає цільову функцію,
  # стартові значення, межі та кількість ітерацій
  func.sa <- optim_sa(fun=func,
                      start=c(0,0),
                      lower=lower,
                      upper=upper,
                      control=list(nlimit=5000))
  print(func.sa)
}
  
  