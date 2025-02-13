library(ggplot2)

# Показникова регресія
exp.reg <- function(x, y, data){
  y.log <- log(y)

  # Обчислення Mx^2, Mx, Mxy, My для знаходження a, b, n
  Mx.2 <- sum(x^2) / length(x)
  Mx <- sum(x) / length(x)
  Mxy <- sum(x * y.log) / length(x)
  My <- sum(y.log) / length(y.log)

  # a, b, n
  n <- (Mxy - Mx * My) / (Mx.2 - Mx * Mx)
  b <- (Mx.2 * My - Mxy * Mx) / (Mx.2 - Mx * Mx)
  a <- exp(b)

  # Формула регресії
  eq <- sprintf("y = %f * e^(%f*x)", round(a, 3), round(n, 3))

  # Ординати регресії
  exp.model.y <- a * exp(1)^(n*x)

  # Обчислення точності (MSE)
  mse <- mean((y - exp.model.y)^2)
  
  return(c(eq, round(mse, 3)))
}


# Графік показникової регресії
exp.plot <- function (p, x, y){
  y.log <- log(y)
  
  Mx.2 <- sum(x^2) / length(x)
  Mx <- sum(x) / length(x)
  Mxy <- sum(x * y.log) / length(x)
  My <- sum(y.log) / length(y.log)

  n <- (Mxy - Mx * My) / (Mx.2 - Mx * Mx)
  b <- (Mx.2 * My - Mxy * Mx) / (Mx.2 - Mx * Mx)
  a <- exp(b)
  
  exp.model.y <- a * exp(1)^(n*x)
  
  p + geom_point(color="Red") +
    geom_line(aes(x = x, y =exp.model.y), color="Blue") +
    labs(title="Exponential Regression", x="x", y="y")

}
