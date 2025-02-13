library(ggplot2)
library(GeneralizedHyperbolic)

x <- c(79.31, 57.43, 60.66, 90.55, 92.12, 71.30, 70.50, 91.25)
y <- c(5.89, 3.84, 6.19, 9.22, 7.87, 6.29, 4.43, 8.91)
data <- data.frame(x, y)

u <- 1/x
u.2 <- u^2
uy <- u*y

Mu <- sum(u) / length(x)
My <- sum(y) / length(x)
Mu.2 <- sum(u.2) / length(x)
Muy <- sum(uy) / length(x)


a <- (Muy - Mu*My) / (Mu.2 - Mu*Mu)
b <- (Mu.2*My - Mu*Muy) / (Mu.2 -Mu*Mu)

eq <- sprintf("y = %f/x + %f", a, b)

hyper.model <- hyperblm(y~x, data=data)
mse <- mean(hyper.model$residuals^2)
mse

p <- ggplot(data, aes(x=x, y=y))
p + geom_point(color="Red") +
  geom_line(aes(x = x, y = fitted.values(hyper.model)), color="Blue") +
  labs(title="Hyperbolic Regression", x="x", y="y")