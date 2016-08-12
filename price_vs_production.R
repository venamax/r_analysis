################################################################
# Directory   : /Users/alejandrorojas/data/lab3_for_students_final
# Dataset: gasOil.Rdata
# Program Name: price_vs_production.R
# Analyst     : Alejandro Rojas
# Last Updated: 8/9/2016 
################################################################
# -------------------------------------------------------------------
# Main Topics Covered:
#
#During 2013 amid high gas prices, the Associated Press (AP) published an article about the U.S. inflation- adjusted price of gasoline and U.S. oil production. 
#The article claims that there is “evidence of no statistical correlation” between oil production and gas prices. 
#The data was not made publicly available, but comparable data was created using data from the Energy Information Administration. 
#The workspace and data frame gasOil.Rdata contains the U.S. oil production (in millions of barrels of oil) and the inflation-adjusted average gas prices (in dollars) over the date range the article indicates.
#In support of their conclusion, the AP reported a single p-value. 
#You have two tasks for this exericse, and both tasks need the use of the data set gasOil.Rdata.
#Your first task is to recreate the analysis that the AP likely used to reach their conclusion. 
#Thoroughly discuss all of the errors the AP made in their analysis and conclusion.
#Your second task is to create a more statistically-sound model that can be used to predict/forecast inflation- adjusted gas prices. 
#Use your model to forecast the inflation-adjusted gas prices from 2012 to 2016.
#####################################################################

#####################################################################
# Setup
# Set working directory
setwd("/Users/alejandrorojas/data/lab3_for_students_final")
getwd()

# Read data
load('gasOil.Rdata')
summary(gasOil)
str(gasOil)

# Visually inspect production and prices
par(mfrow=c(2,2))
plot(gasOil$Production)
hist(gasOil$Production)
plot(gasOil$Price)
hist(gasOil$Price)

# Call libraries
library(sandwich)
library(forecast)
library(statmod)
library(xts)

# Build time series
tail(gasOil$Date)
oil_ts <- ts(gasOil[,c('Production')], 
            start=c(1978,1), 
            end=c(2012,2),
            frequency=12)
gas_ts <- ts(gasOil[,c('Price')], 
             start=c(1978,1), 
             end=c(2012,2),
             frequency=12)

par(mfrow=c(2,1))
plot.ts(oil_ts, type="o",main="Oil Production", ylab='',xlab='', col='Green')
plot.ts(gas_ts, type="o",main="Gas Prices", ylab='',xlab='', col='Red')

### Let's decompose each time series
length(oil_ts)
plot(decompose(oil_ts))
length(gas_ts)
plot(decompose(gas_ts))

## monthly data from 1978 to 2012
## seasonal
## oil production trends downwards
## gas prices more volatile. Double u-shape
## None of them are stationary

## Let's see if we can cointegrate 
## LM1 = actual gas prices are dependant on oil production 
lm1 <- lm(gas_ts ~ oil_ts)
summary(lm1)
cor(oil_ts,gas_ts)
hist(lm1$residuals)
plot(lm1$residuals)
acf(lm1$residuals)
pacf(lm1$residuals)
### LM p-value is 0.575 
### No evidence of a significant relationship
### But underlying residuals are not normal
### We cannot claim any statistical inference

## Let's use diff 
## LM2 = differences in prices dependant on diff on oil production
oil.d <- diff(oil_ts)
gas.d <- diff(gas_ts)
lm2 <- lm(gas.d ~ oil.d)
summary(lm2)
cor(oil.d,gas.d)
## Using LM on the first difference lowers p-value to 0.08
## Let's examine each series
par(mfrow=c(2,2))
plot(oil.d)
hist(oil.d)
plot(gas.d)
hist(gas.d)
### Both now look stationary

## Let's examine each series
par(mfrow=c(2,2))
acf(oil.d)
pacf(oil.d)
acf(gas.d)
pacf(gas.d)
### Looks like seasonality is still affecting data. 

hist(lm2$residuals)
plot(lm2$residuals)
acf(lm2$residuals)
pacf(lm2$residuals)
### Residuals look better but still show correlation problems

## Arima Models.

# Function to find best parameters
get.best.arima <- function(x.ts, test.ts=NA, method='AIC', maxord = c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  best.rmse <- 1e8
  n <- length(x.ts)
  H <- length(test.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
  {
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (method=='RMSE')
      {
        fit.fcast <- forecast.Arima(fit, h=H)
        fit.rmse <- sqrt(mean((fit.fcast$mean-test.ts)**2))
        if (fit.rmse < best.rmse)
        {
          best.rmse <- fit.rmse
          best.aic <- fit.aic
          best.fit <- fit
          best.model <- c(p,d,q,P,D,Q)
        }
      }
      else 
      {
        if (fit.aic < best.aic)
        {
          best.rmse <- NA
          best.aic <- fit.aic
          best.fit <- fit
          best.model <- c(p,d,q,P,D,Q)
        }
      }
    }
  }
  list(best.rmse, best.aic, best.fit, best.model)
}
N <- length(oil_ts)
test <- (N - 51):(N)
train <- 1:(N - 52)
get.best.arima(oil_ts[train], oil_ts[test], method='RMSE')
get.best.arima(gas_ts[train], gas_ts[test], method='RMSE')

# Oil Production Arima (1,1,0)(1,1,0) based on best arima
oil.arima <- arima(oil_ts,
                   order=c(1,1,0),
                   seasonal=list(order=c(1,1,0),
                                 period = 12, 
                                 method='ML')
)
oil.arima
par(mfrow=c(2,2))
hist(oil.arima$residuals)
plot(oil.arima$residuals)
acf(oil.arima$residuals)
pacf(oil.arima$residuals)
oil.fcast<- forecast.Arima(oil.arima, h=12)
oil.fitted <- fitted.Arima(oil.fcast)


# Gas Prices Arima (0,1,0)(1,1,1)
gas.arima <- arima(gas_ts,
                   order=c(0,1,0),
                   seasonal=list(order=c(1,1,1),
                                 period = 12, 
                                 method='ML')
)
gas.arima
par(mfrow=c(2,2))
hist(gas.arima$residuals)
plot(gas.arima$residuals)
acf(gas.arima$residuals)
pacf(gas.arima$residuals)
## residual show correlations. Let's add AR1

# Gas Prices Arima (1,1,0)(1,1,1)
gas.arima <- arima(gas_ts,
                   order=c(1,1,0),
                   seasonal=list(order=c(1,1,1),
                                 period = 12, 
                                 method='ML')
)
gas.arima
par(mfrow=c(2,2))
hist(gas.arima$residuals)
plot(gas.arima$residuals)
acf(gas.arima$residuals)
pacf(gas.arima$residuals)
## ACF Looks better


gas.fcast<- forecast.Arima(gas.arima, h=12)
gas.fitted <- fitted.Arima(gas.fcast)

## Let's plot actual vs fitted by chosen models
par(mfrow=c(2,2))
plot.ts(oil.fitted)
plot.ts(gas.fitted)
plot.ts(oil_ts)
plot.ts(gas_ts)

## Cointegration
## LM3: Fitted gas prices dependent on fitted oil production
lm3 <- lm(gas.fitted ~ oil.fitted)
summary(lm3)
## As LM1 p-value is quite high but both time-series are not stationary

## Let's make them stationary by looking at difference
## LM4: diff on fitted gas prices dependent on diff on fitted oil production
gas.fitted.d <- diff(gas.fitted)
oil.fitted.d <- diff(oil.fitted)
lm4 <- lm(gas.fitted.d~oil.fitted.d)
summary(lm4)
## p-value is now significant at 0.0105
## evidence that an increase in oil production leads to lower gas prices 
hist(lm4$residuals)
plot(lm4$residuals)
acf(lm4$residuals)
pacf(lm4$residuals)
## We still find correlations of residuals but they're closer to white noise
