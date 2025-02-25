func.stochastic <- function(x, y, func){ 
  # межі
  lower <- c(x[1], y[1])
  upper <- c(x[length(x)], y[length(y)])
  # максимальна кількість ітерацій
  N <- 5000
  
  # значення для збереження результатів
  z.min <- NULL
  par.res <- list(0, 0)
  # цикл на N ітерацій
  for (i in 1:N){
    # беремо рандомні x та y
    x.rand <- runif(1, lower[1], upper[1])
    y.rand <- runif(1, lower[2], upper[2])
    # обчислюємо функцію, де x=x.rand, y=y.rand
    z.curr <- func(c(x.rand, y.rand))
    
    if (i == 1) z.min <- z.curr # збереження значення з першого обчислення
    else if (z.min > z.curr){
      # якщо збережене значення більше за поточне,
      # зберігаємо нові значення
      z.min <- z.curr
      par.res <- replace(par.res, 1, x.rand)
      par.res <- replace(par.res, 2, y.rand)
    }
  }
  
  print(paste("Function value: ", z.min))
  print(paste("Parameters: ", par.res[1], " and ", par.res[2]))
  print(paste("Number of iterations: ", N))
}