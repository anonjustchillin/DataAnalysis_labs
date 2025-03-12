source("lab1/linear_reg.R")
source("lab1/quad_reg.R")
source("lab1/geom_reg.R")
source("lab1/exp_reg.R")
source("lab1/log_reg.R")
source("lab1/hyper_reg.R")


x <- c(79.31, 57.43, 60.66, 90.55, 92.12, 71.30, 70.50, 91.25)
y <- c(5.89, 3.84, 6.19, 9.22, 7.87, 6.29, 4.43, 8.91)

cor(x, y, method = "pearson")

data <- data.frame(x, y)
row.1 <- c("Linear", lin.reg(x, y, data)) 
row.2 <- c("Quadratic", quad.reg(x, y, data)) 
row.3 <- c("Geometric", geom.reg(x, y, data)) 
row.4 <- c("Exponential", exp.reg(x, y, data)) 
row.5 <- c("Logarithmic", log.reg(x, y, data)) 
row.6 <- c("Hyperbolic", hyper.reg(x, y, data))
res.data <- data.frame(rbind(row.1, row.2, row.3, row.4, row.5, row.6))
colnames(res.data) <- c("Regression", "Formula", "MSE")
res.data

p <- ggplot(data, aes(x = x, y = y))
lin.plot(p, x, y, data)
quad.plot(p, x, y, data)
geom.plot(p, x, y)
exp.plot(p, x, y)
log.plot(p, x, y, data)
hyper.plot(p, x, y, data)