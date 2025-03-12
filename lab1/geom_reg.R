library(ggplot2)

# Степенева регресія
geom.reg <- function(x, y, data){
  x.log <- log(x)
  y.log <- log(y)

  # Обчислення Mx^2, Mx, Mxy, My для знаходження a, b, n
  Mx.2 <- sum(x.log^2) / length(x.log)
  Mx <- sum(x.log) / length(x.log)
  Mxy <- sum(x.log * y.log) / length(x.log)
  My <- sum(y.log) / length(x.log)

  # a, b, n
  n <- (Mxy - Mx * My) / (Mx.2 - Mx * Mx)
  b <- (Mx.2 * My - Mxy * Mx) / (Mx.2 - Mx * Mx)
  a <- exp(b)

  # Формула регресії
  eq <- sprintf("y = %f * x^%f", round(a, 3), round(n, 3))

  # Ординати регресії
  geom.model.y <- a*x^n

  # Обчислення точності (MSE)
  mse <- mean((y - geom.model.y)^2)

  geom.model <- lm(geom.model.y~x, data=data)

  print(summary(geom.model))

  return (c(eq, round(mse, 3)))
}


# Графік степеневої регресії
geom.plot <- function (p, x, y){
  x.log <- log(x)
  y.log <- log(y)

  Mx.2 <- sum(x.log^2) / length(x.log)
  Mx <- sum(x.log) / length(x.log)
  Mxy <- sum(x.log * y.log) / length(x.log)
  My <- sum(y.log) / length(x.log)

  n <- (Mxy - Mx * My) / (Mx.2 - Mx * Mx)
  b <- (Mx.2 * My - Mxy * Mx) / (Mx.2 - Mx * Mx)
  a <- exp(b)

  geom.model.y <- a*x^n
  
  p + geom_point(color="Red") +
    geom_line(aes(x = x, y = geom.model.y), color="Blue") +
    labs(title="Geometric Regression", x="x", y="y")

}