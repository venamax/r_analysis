################################################################
# Directory   : /Users/alejandrorojas/data/lab3_for_students_final
# Dataset: google_correlate_flight.csv
# Program Name: flights_demand.R
# Analyst     : Alejandro Rojas
# Last Updated: 8/9/2016 
################################################################
# -------------------------------------------------------------------
# Main Topics Covered:
#
# Part 1 (50 points): Forecast the Web Search Activity for global Warming
#The file, google_correlate_flight.csv, contains the relative web search activity for the phrase “flight prices” over time. 
#Your data science team believes that search activity for this phrase is positively correlated with consumer demand for flying (and possibly prices). 
#Your task is to forecast the relative demand of this phrase for the year 2016. 
#For the purposes of this assignment, ignore the units of the data as they are not relevant here.
#Remember to explain and justify each of your modelling decisions. 
#Also, comment on your forecast. 
#Do you notice anything interesting? Do you notice anything worth worrying about?
#####################################################################

#####################################################################
# Setup

# Set working directory
setwd("/Users/alejandrorojas/data/lab3_for_students_final")
getwd()

# Read csv
f <- read.csv('google_correlate_flight')
# Observe dataset
summary(f)
str(f)
# Visually inspect flight prices
par(mfrow=c(2,1))
plot(f$flight.prices)
hist(f$flight.prices)

# Call libraries
library(sandwich)
library(forecast)
library(statmod)
library(xts)

# Build time series
fp_ts <- ts(f[,c('flight.prices')], 
   start=c(2004,1), 
   end=c(2016,1),
   frequency=52)
par(mfrow=c(1,1))
plot.ts(fp_ts, type="o",main="Flight Prices Web Search", ylab='',xlab='Time', col='Blue')
length(fp_ts)
plot(decompose(fp_ts))
## 625 weeks from 2004 to 2016
## No clear trend but data looks seasonal

# Visually inspect autocorelations functions
par(mfrow=c(2,1))
acf(fp_ts)
pacf(fp_ts)

# Let's start with a AR model
ar.fp <- ar(fp_ts, method='mle')
## Does not converge because of seasonal components

# Let's include seasonal components
ar2s1 <- arima(fp_ts,order=c(2,0,0),seasonal=list(order=c(1,0,0),period=52, method='CSS'))
ar2s1$aic
## Let's examine residuals
par(mfrow=c(2,2))
plot.ts(ar2s1$residuals)
hist(ar2s1$residuals)
acf(ar2s1$residuals)
pacf(ar2s1$residuals)
## Looks like a good model aic = 1171

# Let's try ar order 2 for seasonal
ar2s2 <- arima(fp_ts,order=c(2,0,0),seasonal=list(order=c(2,0,0),period=52))
ar2s2$aic
### Does not converge

#Let's evaluate arma models
ar2s1ma1 <- arima(fp_ts,order=c(2,0,1),seasonal=list(order=c(1,0,0),period=52))
ar2s1ma1$aic
## aic drops a bit to 1162

# Let's try a ma 2 model
ar2s1ma2 <- arima(fp_ts,order=c(2,0,2),seasonal=list(order=c(1,0,0),period=52))
ar2s1ma2$aic
## No big change aic = 1162

# Let's now try an integrated model
ar2s1i1 <- arima(fp_ts,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=52))
ar2s1i1$aic
## No big change aic = 1141

#Let's try ma only model with ma seasonality
ma1s1 <- arima(fp_ts, order=c(0,0,1), seasonal=list(order=c(0,0,1),period=52),method='ML')
ma1s1$aic

# Let's try (1,1,1)(0,1,1)
alternate <-  arima(fp_ts, order=c(1,1,1), seasonal=list(order=c(0,1,1),period=52),method='ML')
alternate$aic

# summary 
summary(ar2s1$residuals)
hist(ar2s1$residuals)

# make forecast 
fp.fcast <- forecast.Arima(ar2s1, h=52)
fp.fcast<- forecast.Arima(alternate, h=52)

# Plot forecast vs original
dev.off()
par(mfrow=c(1,1))
xlimits <- c(2004, 2017)
ylimits <- c(-3, 6)
plot(fp.fcast, lty=2, xlim=xlimits,ylim=ylimits,
     main="Out-of-Sample Forecast",
     ylab="Original, Estimated, and Forecast Values")
par(new=T)
plot.ts(fitted(fp.fcast), 
        col="green",lty=1,axes=F, xlim=xlimits,ylim=ylimits,ylab='')
par(new=T)
plot.ts(fp_ts, col="gray",axes=F,xlim=xlimits,ylim=ylimits,ylab="", lty=2)

# add legend
leg.txt <- c("Original Series", "Fitted series", "Forecast")
legend("topleft", legend=leg.txt, lty=c(2,1,1),
       col=c("gray","green","blue"), lwd=c(1,1,2),
       bty='n', cex=1)

#### Conclusion
# We expect demand to drop as the year unfolds. So it looks like
# pricing will probably needs to be more aggressive to entice customers
# and reach higher capacity utilization in available routes
