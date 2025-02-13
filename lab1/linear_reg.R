library(ggplot2)

# Лінійна регресія
lin.reg <- function(x, y, data) { 
  # Обчислення Mx^2, Mx, Mxy, My для знаходження a та b
  Mx.2 <- sum(x^2) / length(x)
  Mx <- sum(x) / length(x)
  Mxy <- sum(x * y) / length(x)
  My <- sum(y) / length(y)

  # Коефіцієнти a та b
  a <- (Mxy - Mx * My) / (Mx.2 - Mx * Mx)
  b <- (Mx.2 * My - Mxy * Mx) / (Mx.2 - Mx * Mx)

  # Формула регресії
  eq <- sprintf("y = %f * x + %f", round(a, 3), round(b, 3))

  # Лінійна модель, яка містить ординати регресії та R^2 для точності
  lin.model <- lm(y ~ x, data = data)
  
  # Обчислення точності (MSE)
  mse <- mean(lin.model$residuals^2)
  
  return (c(eq, round(mse, 3)))
}


# Графік лінійної регресії
lin.plot <- function (p, x, y, data){
  lin.model <- lm(y ~ x, data = data)
  p +
    geom_point(color = "Red") +
    geom_line(aes(x = x, y = predict(lin.model)), color = "Blue") +
    labs(title = "Linear Regression", x = "x", y = "y")
  
}