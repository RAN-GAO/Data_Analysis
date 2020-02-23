# Data Analysis_Time Series
# by Ran Gao on 05/12/2018 

# For this problem we will be working with a data set containing tractor sales over time.
data = read.csv('Tractor-Sales.csv')

# 1. The first step is to download the data as above and explore.
# (1) Understand what the data is and explore the data.
View(data)
# It is a data with the number of tractor sold from Jan 3rd to Dec 14th.
summary(data$Number.of.Tractor.Sold)

# (2) Create a time sereis object using ts().
data = ts(data[,2], start=c(2003,1), frequency=12)
data

# (3) Explore the time series with functions.
plot(data)
start(data)
end(data)
frequency(data)
window(data)

# (3.1) Plot the time series and three smoothed versions of the time series
#       using ma() and three different odd numbers for k.
library(forecast)
ylim <- c(min(data),max(data))
plot(data, main="tractor sales over time")
plot(ma(data, 3), main="Simple Moving Averages(k=3)", ylim=ylim)
plot(ma(data, 7), main="Simple Moving Averages(k=7)", ylim=ylim)
plot(ma(data, 15), main="Simple Moving Averages(k=15)", ylim=ylim)
# According to the plots, it is apparently that as k increases,
# the plot becomes increasingly smoothed.
# Also, it also shows an obvious trend in the plot.

# (3.2) Think about the data and whether or not it may have a seasonal aspect.
#       Look at the plot of your time series and decide what combination of
#       trend, seasonal and irregular components the time series may have.
# According to the data and the plot, I think it may have a seasonal term.
# I also think that it should be a multiplicative model of trend, seasonal and irregular components.

# (3.3) Use stl() to decompose the time series.
#       Recall that stl() can only handle additive models so need to do a log transformation.
ldata <- log(data)
plot(ldata, ylab="log of the tractors sales.")

# (3.4) View the components for each observations numerically and pictiorially using.
fit <- stl(ldata, s.window="period")
plot(fit)
fit$time.series
# 1. The trend term is about 5 and is pretty stationary.
# 2. The remainder is not that large, which means the model is pretty good.

# (3.5) If the data has a seasonal aspect to it then use monthplot() and seasonplot()
#       to help visualize the seasonal decomposition other points of view.
library(forecast)
monthplot(data, xlab="", ylab="")
seasonplot(data, year.labels="TRUE", main="")

# 2. For this problem we will continue with the tractor data set.

# (1) Using ets() and specifying the 'model' parameter based on your time series' error type.
#     trend type and seasonal type, fit the model and make a 1-step ahead forecast.
# I choose to use the tripl exponential model.
library(forecast)
fit <- ets(log(data), model="AAA")
fit
pred <- forecast(fit, 1)

plot(pred, main="Forecast for tractor sales.")
pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)

p <- cbind(pred$mean,pred$lower,pred$upper)
dimnames(p)[[2]] <- c("mean","Lo 80","Lo 95","Hi 80","Hi 95")
p
# (2) Look at the output.

# (2.1) Based on the the alpha, what can you tell about distant vs most recent observations being considered in the forecast.
#       ALpha is about 0.7, it means should care about distant and recent for the level renewing.

# (2.2) What is the forecast?
#       The forecast is 563.9375.

# (2.3) What is the 95% confidence interval for this forecast?
#       [525.5816,605.0926]

# 3. ARIMA forecasting models.
# For this problem we will be using the yearly earnings data in "Earnings.txt".
earnings <- read.table("D:/R/Earnings.txt",
                       sep=",", 
                       col.names=c("id", "earning"))
View(earnings)
earning <- ts(earnings$earning,frequency=1)
earning

# (1) Ensure that the time series is stationary.
# (1a) Plot the time series.
plot(earning)
# It seems not that stable and transform with log function.

# (1b) Plot the adjusted time series.
plot(log(earning))
learning <- log(earning)
dearning <- diff(learning)
plot(dearning)

# (1c) Check if the resulting time series is indeed stationary by applying the ADF test.
library(forecast)
library(tseries)
adf.test(dearning)
# The p-value is 0.01 so it is stationary.

# (2) Identifying one or more reasonable models with the transformed time series.
# (2a) Create the ACF and the PACF plots.
Acf(dearning)
Pacf(dearning)

# (2b) According to the plots and table in the book, p=q=1.

# (3) Note that there are a few additional steps for seaonal terms.

# (4) Fit your ARIMA model using the original data set.
# (4a) Fit your model.
library(forecast)
fit <- arima(learning, order=c(1,1,1))
fit

# (4b) Look at the model accuracy() and identity the mean absolute percent error.
# mae = 0.1214483
accuracy(fit)

# (4c) Evaluate the model with qqnorm(), qqline(), and Box.test().
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals,type="Ljung-Box")

# (4d) Look at the resulting auto correlations using.
acf(ts(fit$residuals),main="ACF Residual")
acf(ts(fit$residuals),main="PACF Residual")

# (5) Forecast 4 steps into the future and plot your results.
forecast(fit, 4)
plot(forecast(fit, 4))