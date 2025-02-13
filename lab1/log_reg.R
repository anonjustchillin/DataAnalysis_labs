library(ggplot2)

log.reg <- function(x, y, data){
  x.log <- log(x)

  Mx.2 <- sum(x.log^2) / length(x.log)
  Mx <- sum(x.log) / length(x.log)
  Mxy <- sum(x.log * y) / length(x.log)
  My <- sum(y) / length(y)

  a <- (Mxy - Mx * My) / (Mx.2 - Mx * Mx)
  b <- (Mx.2 * My - Mxy * Mx) / (Mx.2 - Mx * Mx)

  eq <- sprintf("y = %f * ln(x) + %f", round(a, 3), round(b, 3))

  log.model <- lm(y~log(x), data=data)
  mse <- mean(log.model$residuals^2)
  
  return (c(eq, round(mse, 3)))
}

log.plot <- function (p, x, y, data){
  log.model <- lm(y~log(x), data=data)
  p + geom_point(color="Red") +
    geom_line(aes(x = x, y = predict(log.model)), color="Blue") +
    labs(title="Logarithmic Regression", x="x", y="y")

}