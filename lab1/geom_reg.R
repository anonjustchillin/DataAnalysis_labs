library(ggplot2)

geom.reg <- function(x, y, data){
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

  eq <- sprintf("y = %f * x^%f", round(a, 3), round(n, 3))
  
  mse <- mean((y - geom.model.y)^2)
  
  return(c(eq, round(mse, 3)))
}

geom.plot <- function (p, x, y, data){
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