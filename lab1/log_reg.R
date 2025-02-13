library(ggplot2)

x <- c(79.31, 57.43, 60.66, 90.55, 92.12, 71.30, 70.50, 91.25)
y <- c(5.89, 3.84, 6.19, 9.22, 7.87, 6.29, 4.43, 8.91)
data <- data.frame(x, y)

x.log <- log(x)

Mx.2 <- sum(x.log^2) / length(x.log)
Mx <- sum(x.log) / length(x.log)
Mxy <- sum(x.log * y) / length(x.log)
My <- sum(y) / length(y)

a <- (Mxy - Mx * My) / (Mx.2 - Mx * Mx)
b <- (Mx.2 * My - Mxy * Mx) / (Mx.2 - Mx * Mx)

eq <- sprintf("y = %f * ln(x) + %f", a, b)

log.model <- lm(y~x.log, data=data)
mse <- mean(log.model$residuals^2)
mse

p <- ggplot(data, aes(x=x, y=y))
p + geom_point(color="Red") +
  geom_line(aes(x = x, y = predict(log.model)), color="Blue") +
  labs(title="Linear Regression", x="x", y="y")