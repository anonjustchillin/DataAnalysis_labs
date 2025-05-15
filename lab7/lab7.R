library(ggplot2)
library(forecast)
library(timetk)

set.seed(123)

# Data preparation
path <- 'D:\\uni\\2курс\\Аналіз_даних\\av_temp.csv'
data <- read.csv(path, stringsAsFactors = F)
head(data)

data <- data[-c(1, 2, 3),]

colnames(data) <- c('Year', 'Temperature')

sum(is.na(data))
summary(data)

data$Temperature = as.double(data$Temperature)

data$Year = as.integer(data$Year)
data$Year = data$Year %/% 100

ts.data <- ts(data$Temperature,
              start=data$Year[1],
              end=data$Year[nrow(data)],
              frequency=1)
ts.data

# Graphs
ts.plot(ts.data, col='blue')
abline(reg=lm(ts.data~time(ts.data)), col='red')

# ACF and PACF

acf(ts.data, main='ACF for Temperature')
pacf(ts.data, main='PACF for Temperature')


# ARMA
arma.mod <- arima(ts.data, order=c(2,0,1))
print(arma.mod)

ts.plot(ts.data)
points(ts.data - residuals(arma.mod), type="l", col=2, lty=2)

arma.forecast <- predict(arma.mod, n.ahead = 10)
print(arma.forecast)

ts.plot(ts.data, xlim = c(1980, 2034))
points(arma.forecast$pred, type="l", col=2)
points(arma.forecast$pred - 2*arma.forecast$se, type="l", col=2, lty=2)
points(arma.forecast$pred + 2*arma.forecast$se, type="l", col=2, lty=2)

AIC(arma.mod)
BIC(arma.mod)

accuracy(arma.mod)


# ARIMA
arima.mod <- arima(ts.data, order=c(2,1,1))
print(arima.mod)

ts.plot(ts.data)
points(ts.data - residuals(arima.mod), type="l", col=2, lty=2)

arima.forecast <- predict(arima.mod, n.ahead = 10)
print(arima.forecast)

ts.plot(ts.data, xlim = c(1980, 2034))
points(arima.forecast$pred, type="l", col=2)
points(arima.forecast$pred - 2*arima.forecast$se, type="l", col=2, lty=2)
points(arima.forecast$pred + 2*arima.forecast$se, type="l", col=2, lty=2)

AIC(arima.mod)
BIC(arima.mod)

accuracy(arima.mod)


# Moving average (simple)
sma.mod <- TTR::SMA(ts.data, n = 3)
print(sma.mod)

plot(ts.data, col = "blue", main = "Simple Moving Average")
lines(sma.mod, col = "red")
legend("topright", legend = c("Original", "SMA"),
       col = c("blue", "red"), lty = 1)

MAE <- sapply(abs(ts.data - sma.mod), mean, na.rm=TRUE)
MAE
MSE <- sapply((ts.data - sma.mod)^2, mean, na.rm=TRUE)
MSE
RMSE <- sqrt(MSE)
RMSE


# Moving average (exp)
ema.mod <- stats::filter(ts.data, filter = 0.1, method = "recursive")
print(ema.mod)

plot(ts.data, col = "blue", main = "Exponential Moving Average")
lines(ema.mod, col = "red")
legend("topright", legend = c("Original", "EMA"),
       col = c("blue", "red"), lty = 1)

MAE <- sapply(abs(ts.data - ema.mod), mean, na.rm=TRUE)
MAE
MSE <- sapply((ts.data - ema.mod)^2, mean, na.rm=TRUE)
MSE
RMSE <- sqrt(MSE)
RMSE


# Exponential Smoothing (single)
train.size <- as.integer(nrow(data)*0.8)

train.ts <- subset(ts.data, start=1, end=train.size)
train.ts
test.ts <- subset(ts.data, start=train.size, end=nrow(data))
test.ts

hw.mod <- HoltWinters(ts.data, alpha=0.25, beta=FALSE, gamma=FALSE)
str(hw.mod)
hw.mod$fitted

plot(hw.mod, col='blue')
legend("topright", c("Train data","Smoothed data"), lty=8, col=c("blue","red"), cex=0.7)


hw.forecast <- forecast(hw.mod, h=20)
hw.forecast

plot(hw.forecast, col="blue")

SSE <- hw.mod$SSE
SSE
MSE <- SSE/length(ts.data)
MSE
RMSE <- sqrt(MSE)
RMSE


