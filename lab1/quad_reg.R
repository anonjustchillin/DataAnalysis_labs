library(Ryacas)
library(ggplot2)

# Квадратична регресія
quad.reg <- function(x, y, data){
  # Обчислення Mx^4, Mx^3, Mx^2, Mx, Mxy, My для знаходження a та b
  M.4 <- sum(x^4) / length(x)
  M.3 <- sum(x^3) / length(y)
  M.2 <- sum(x^2) / length(x)
  M.1 <- sum(x) / length(x)
  Mx.2y <- sum(x^2 * y) / length(x)
  Mxy <- sum(x * y) / length(x)
  My <- sum(y) / length(y)

  # Обчислення системи рівнянь за допомогою solve(),
  # яка бере матрицю з коефіцієнтів та вектор значень, чому дорівнюють рівняння
  eqs <- rbind(c(M.4, M.3, M.2),
               c(M.3, M.2, M.1),
               c(M.2, M.1, 1))
  eq.res <- c(Mx.2y, Mxy, My)
  sol <- solve(eqs, eq.res)

  # Коефіцієнти a, b, c
  a <- sol[1]
  b <- sol[2]
  c <- sol[3]

  # Формула регресії
  eq <- sprintf("y = %f * x^2 + %f * x + %f", round(a, 3), round(b, 3), round(c, 3))

  # Квадратична модель, яка містить ординати регресії та R^2 для точності
  quad.model <- lm(y~x + I(x^2), data=data)

  # Обчислення точності (MSE)
  mse <- mean(quad.model$residuals^2)

  print(summary(quad.model))

  return (c(eq, round(mse, 3)))
}


# Графік квадратичної регресії
quad.plot <- function (p, x, y, data){
  quad.model <- lm(y~x + I(x^2), data=data)
  p + geom_point(color="Red") +
    geom_line(aes(x = x, y = predict(quad.model)), color="Blue") +
    labs(title="Quadratic Regression", x="x", y="y")

}