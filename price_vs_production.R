################################################################
# Directory   : /Users/alejandrorojas/data/lab3_for_students_final
# Dataset: gasOil.Rdata
# Program Name: flights_demand.R
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
lm1 <- lm(gas_ts ~ oil_ts)
summary(lm1)
cor(oil_ts,gas_ts)
### LM p-value is 0.575 
### No evidence of a significant relationship

## Let's use diff 
oil.d <- diff(oil_ts)
gas.d <- diff(gas_ts)
lm2 <- lm(gas.d ~ oil.d)
summary(lm2)
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
### Looks like seasonality is affecting data





