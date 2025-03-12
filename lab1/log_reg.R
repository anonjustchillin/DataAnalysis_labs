library(ggplot2)

# Логарифмічна регресія
log.reg <- function(x, y, data){
  x.log <- log(x)

  # Обчислення Mx^2, Mx, Mxy, My для знаходження a та b
  Mx.2 <- sum(x.log^2) / length(x.log)
  Mx <- sum(x.log) / length(x.log)
  Mxy <- sum(x.log * y) / length(x.log)
  My <- sum(y) / length(y)

  # Коефіцієнти a та b
  a <- (Mxy - Mx * My) / (Mx.2 - Mx * Mx)
  b <- (Mx.2 * My - Mxy * Mx) / (Mx.2 - Mx * Mx)

  # Формула регресії
  eq <- sprintf("y = %f * ln(x) + %f", round(a, 3), round(b, 3))

  # Логарифмічна модель, яка містить ординати регресії та R^2 для точності
  log.model <- lm(y~log(x), data=data)

  # Обчислення точності (MSE)
  mse <- mean(log.model$residuals^2)

  print(summary(log.model))

  return (c(eq, round(mse, 3)))
}

# Графік логарифмічної регресії
log.plot <- function (p, x, y, data){
  log.model <- lm(y~log(x), data=data)
  p + geom_point(color="Red") +
    geom_line(aes(x = x, y = predict(log.model)), color="Blue") +
    labs(title="Logarithmic Regression", x="x", y="y")

}