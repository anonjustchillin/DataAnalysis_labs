library(ggplot2)
library(GeneralizedHyperbolic)

hyper.reg <- function(x, y, data){
  u <- 1/x
  u.2 <- u^2
  uy <- u*y

  Mu <- sum(u) / length(x)
  My <- sum(y) / length(x)
  Mu.2 <- sum(u.2) / length(x)
  Muy <- sum(uy) / length(x)


  a <- (Muy - Mu*My) / (Mu.2 - Mu*Mu)
  b <- (Mu.2*My - Mu*Muy) / (Mu.2 -Mu*Mu)

  eq <- sprintf("y = %f/x + %f", round(a, 3), round(b, 3))

  hyper.model <- hyperblm(y~x, data=data)
  mse <- mean(hyper.model$residuals^2)
  
  return (c(eq, round(mse, 3)))
}

hyper.plot <- function (p, x, y, data){
  hyper.model <- hyperblm(y~x, data=data)
  p + geom_point(color="Red") +
    geom_line(aes(x = x, y = fitted.values(hyper.model)), color="Blue") +
    labs(title="Hyperbolic Regression", x="x", y="y")

}