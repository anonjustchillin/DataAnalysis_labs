library(ggplot2)

lin.reg <- function(x, y, data)
{ Mx.2 <- sum(x^2) / length(x)
  Mx <- sum(x) / length(x)
  Mxy <- sum(x * y) / length(x)
  My <- sum(y) / length(y)

  a <- (Mxy - Mx * My) / (Mx.2 - Mx * Mx)
  b <- (Mx.2 * My - Mxy * Mx) / (Mx.2 - Mx * Mx)

  eq <- sprintf("y = %f * x + %f", round(a, 3), round(b, 3))

  lin.model <- lm(y ~ x, data = data)
  mse <- mean(lin.model$residuals^2)
  
  return (c(eq, round(mse, 3)))
}

lin.plot <- function (p, x, y, data){
  lin.model <- lm(y ~ x, data = data)
  p +
    geom_point(color = "Red") +
    geom_line(aes(x = x, y = predict(lin.model)), color = "Blue") +
    labs(title = "Linear Regression", x = "x", y = "y")
  
}