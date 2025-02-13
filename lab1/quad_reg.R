library(Ryacas)
library(ggplot2)

quad.reg <- function(x, y, data){
  M.4 <- sum(x^4) / length(x)
  M.3 <- sum(x^3) / length(y)
  M.2 <- sum(x^2) / length(x)
  M.1 <- sum(x) / length(x)
  Mx.2y <- sum(x^2 * y) / length(x)
  Mxy <- sum(x * y) / length(x)
  My <- sum(y) / length(y)

  eqs <- rbind(c(M.4, M.3, M.2),
               c(M.3, M.2, M.1),
               c(M.2, M.1, 1))
  eq.res <- c(Mx.2y, Mxy, My)

  sol <- solve(eqs, eq.res)
  sol

  a <- sol[1]
  b <- sol[2]
  c <- sol[3]

  eq <- sprintf("y = %f * x^2 + %f * x + %f", round(a, 3), round(b, 3), round(c, 3))

  quad.model <- lm(y~x + I(x^2), data=data)
  mse <- mean(quad.model$residuals^2)

  return (c(eq, round(mse, 3)))
}

quad.plot <- function (p, x, y, data){
  quad.model <- lm(y~x + I(x^2), data=data)
  p + geom_point(color="Red") +
    geom_line(aes(x = x, y = predict(quad.model)), color="Blue") +
    labs(title="Quadratic Regression", x="x", y="y")

}