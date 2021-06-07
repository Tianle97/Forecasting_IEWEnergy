##############################################
#To start off we can clear all the variables
#from the current environment
#and close all the plots.
##############################################

rm(list = ls())
graphics.off()


library(aimsir17)
library(astsa)
library(dplyr)
library(forecast)
library(ggplot2)
library(MLmetrics)
library(tseries)


# pick up daily weather(msl,wdsp and rainfall)
MH_weather <- observations %>%
  group_by(month,day) %>%
  summarise(Time=first(date),
            maxWdsp=max(wdsp,na.rm = T),
            station = "MACE HEAD",
            MaxTemp=max(temp))


MH_train <- ts(MH_weather,start=1,end = 300)
MH_test  <- ts(MH_weather,start=301,end = 365)

covariates <- c("MaxTemp")

arima_MH <- auto.arima(MH_train[,"maxWdsp"],xreg =MH_train[,covariates] )
arima_MH

plot(forecast(arima_MH,xreg =MH_test[,covariates]))



