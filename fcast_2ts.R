################################################################
# Directory   : /Users/alejandrorojas/data/lab3_for_students_final
# Dataset: ex3series.csv
# Program Name: fcast_2ts.R
# Analyst     : Alejandro Rojas
# Last Updated: 8/9/2016 
################################################################
# -------------------------------------------------------------------
# Main Topics Covered:
#
#Part 3 (50 Points): Forecast two series
#The flie ex3series.csv contains two monthly time series. 
#You task is to (1) build a forecasting model using techniques covered in lecture 8 - 13, which includes the async. lectures, live sessions, and the assigned readings, and (2) produce a 6-step ahead (monthly) forecast. 
# Please follow the instructions outlined above. To repeat,
#• Your analysis needs to include a comprehensive graphical analysis
#• Your analysis needs to be accompanied by detailed narrative. Just printing a bunch of graphs and
#econometric results will likely receive a very low score, if not a score of zero.
#• Your analysis needs to show that your models are valid (in statistical sense).
#• Your rationale of using certian metrics to choose models need to be provided. Explain the validity /
#  pros / cons of the metric you use to choose your “best” model.
#• Your rationale of any decisions made in your modeling needs to be explained and supported with
#empirical evidence.
#• All the steps to arrive at your final model need to be shown and explained clearly.
#• All of the assumptions of your final model need to be thoroughly tested and explained and shown to be
#valid. Don’t just write something like, “the plot looks reasonable”, or “the plot looks good”, as different people interpret vague terms like “reasonable” or “good” differently.
#####################################################################

# Setup
# Set working directory
setwd("/Users/alejandrorojas/data/lab3_for_students_final")
getwd()

# Read csv
ts2 <- read.csv('ex3series.csv')
summary(ts2)
str(ts2)

# Visually inspect flight prices
par(mfrow=c(2,2))
plot(ts$series1)
hist(ts$series1)
plot(ts$series2)
hist(ts$series2)

# Call libraries
library(sandwich)
library(forecast)
library(statmod)
library(xts)

# Build time series
x_ts <- ts(ts2[,c('series1')], 
            #start=c(,), 
            #end=c(,),
            frequency=12)
y_ts <- ts(ts2[,c('series2')], 
           #start=c(,), 
           #end=c(,),
           frequency=12)
par(mfrow=c(2,2))
plot.ts(x_ts, type="o",main="Series 1", ylab='',xlab='Time', col='Blue')
hist(x_ts, col='Blue')
plot.ts(y_ts, type="o",main="Series 2", ylab='',xlab='Time', col='Red')
hist(y_ts, col='Red')

length(x_ts)
plot(decompose(x_ts))
plot(decompose(y_ts))

## Data has NA
str(x_ts)
summary(x_ts)
summary(y_ts)
x_ts <- x_ts[1:125]
y_ts <- y_ts[1:125]

# Let's examine their ACF
# Visually inspect autocorelations functions
par(mfrow=c(2,2))
acf(x_ts)
pacf(x_ts)
acf(y_ts)
pacf(y_ts)

# Let's explore using AR
ar(x_ts)
## order 2
ar(y_ts)
## order 14

# Arima models
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
get.best.arima(x_ts, x_ts, method='RMSE')
get.best.arima(y_ts, y_ts, method='RMSE')

# x_ts (1,0,1)(1,0,1)
# y_ts (0,0,1)(1,0,0)

# x_ts Arima (1,0,1)(1,0,1) based on best arima
# x_ts Arima (2,0,0)(0,0,1) after looking at residuals 
x.arima <- arima(x_ts,
                   order=c(2,0,0),
                   seasonal=list(order=c(0,0,1),
                                 period = 3, 
                                 method='ML')
)
x.arima


par(mfrow=c(2,2))
hist(x.arima$residuals)
plot(x.arima$residuals)
acf(x.arima$residuals)
pacf(x.arima$residuals)
x.fcast6<-forecast.Arima(x.arima, h=6)
x.fcast<- forecast.Arima(x.arima, h=12)
x.fitted <- fitted.Arima(x.fcast)


# y_ts Arima (0,0,1)(1,0,0) based on best arima
# y_ts Arima (2,0,0)(0,0,4) based on ACF closer to white noise 
y.arima <- arima(y_ts,
                 order=c(2,0,0),
                 seasonal=list(order=c(0,0,4),
                               period = 12, 
                               method='ML')
)
y.arima


par(mfrow=c(2,2))
hist(y.arima$residuals)
plot(y.arima$residuals)
acf(y.arima$residuals)
pacf(y.arima$residuals)
y.fcast6<-forecast.Arima(y.arima, h=6)
y.fcast<- forecast.Arima(y.arima, h=12)
y.fitted <- fitted.Arima(y.fcast)


# Predict time series 1
summary(x_ts)
length(x_ts)
dev.off()
par(mfrow=c(1,1))
xlimits <- c(1,131)
ylimits <- c(400, 2300)
plot(x.fcast6, lty=2, xlim=xlimits,ylim=ylimits,
     main="Out-of-Sample Forecast",
     ylab="Original, Estimated, and Forecast Values")
par(new=T)
plot.ts(fitted(x.fcast), 
        col="green",lty=1,axes=F, xlim=xlimits,ylim=ylimits,ylab='')
par(new=T)
plot.ts(x_ts, col="gray",axes=F,xlim=xlimits,ylim=ylimits,ylab="", lty=2)

# add legend
leg.txt <- c("Original Series", "Fitted series", "Forecast")
legend("topleft", legend=leg.txt, lty=c(2,1,1),
       col=c("gray","green","blue"), lwd=c(1,1,2),
       bty='n', cex=1)

# Predict time series 2
summary(y_ts)
length(y_ts)
dev.off()
par(mfrow=c(1,1))
xlimits <- c(1,131)
ylimits <- c(500, 1100)
plot(y.fcast6, lty=2, xlim=xlimits,ylim=ylimits,
     main="Out-of-Sample Forecast",
     ylab="Original, Estimated, and Forecast Values")
par(new=T)
plot.ts(fitted(y.fcast), 
        col="green",lty=1,axes=F, xlim=xlimits,ylim=ylimits,ylab='')
par(new=T)
plot.ts(y_ts, col="gray",axes=F,xlim=xlimits,ylim=ylimits,ylab="", lty=2)

# add legend
leg.txt <- c("Original Series", "Fitted series", "Forecast")
legend("topleft", legend=leg.txt, lty=c(2,1,1),
       col=c("gray","green","blue"), lwd=c(1,1,2),
       bty='n', cex=1)