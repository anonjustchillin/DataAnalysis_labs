library(Ryacas)
library(ggplot2)

x <- c(79.31, 57.43, 60.66, 90.55, 92.12, 71.30, 70.50, 91.25)
y <- c(5.89, 3.84, 6.19, 9.22, 7.87, 6.29, 4.43, 8.91)
data <- data.frame(x, y)

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

eq <- sprintf("y = %f * x^2 + %f * x + %f", a, b, c)

quad.model <- lm(y~x + I(x^2), data=data)
summary(quad.model)

mse <- mean(quad.model$residuals^2)
mse

p <- ggplot(data, aes(x=x, y=y))
p + geom_point(color="Red") +
  geom_line(aes(x = x, y = predict(quad.model)), color="Blue") +
  labs(title="Quadratic Regression", x="x", y="y")