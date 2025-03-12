library(ggplot2)
library(GeneralizedHyperbolic)

# Гіперболічна регресія
hyper.reg <- function(x, y, data){
  u <- 1/x
  u.2 <- u^2
  uy <- u*y

  # Обчислення Mu, My, Mu^2, Muy для знаходження a та b
  Mu <- sum(u) / length(x)
  My <- sum(y) / length(x)
  Mu.2 <- sum(u.2) / length(x)
  Muy <- sum(uy) / length(x)

  # Коефіцієнти a та b
  a <- (Muy - Mu*My) / (Mu.2 - Mu*Mu)
  b <- (Mu.2*My - Mu*Muy) / (Mu.2 -Mu*Mu)

  # Формула регресії
  eq <- sprintf("y = %f/x + %f", round(a, 3), round(b, 3))

  # Гіперболічна модель, яка містить ординати регресії та R^2 для точності
  hyper.model <- hyperblm(y~x, data=data)

  # Обчислення точності (MSE)
  mse <- mean(hyper.model$residuals^2)

  print(summary(hyper.model))
  print(residuals(hyper.model)^2)

  return (c(eq, round(mse, 3)))
}

# Графік гіперболічної регресії
hyper.plot <- function (p, x, y, data){
  hyper.model <- hyperblm(y~x, data=data)
  p + geom_point(color="Red") +
    geom_line(aes(x = x, y = fitted.values(hyper.model)), color="Blue") +
    labs(title="Hyperbolic Regression", x="x", y="y")
}