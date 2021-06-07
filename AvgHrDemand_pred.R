##############################################
#To start off we can clear all the variables
#from the current environment
#and close all the plots.
##############################################

rm(list = ls())
graphics.off()

library("aimsir17")
library("astsa")
library("dplyr")
library("forecast")
library("ggplot2")
library("MLmetrics")
library("tseries")
######################################################

# use Arima model to forecasting hourly IEDemand

######################################################


hr_demand <- eirgrid17 %>%
  group_by(month,day,hour) %>%
  summarise(Time=first(date),
            AvrHrDemand=mean(IEDemand),
            AvrWindGen=mean(IEGeneration))

# pick up one month data for training
hr_test <- filter(hr_demand,month ==1)
real_data <- filter(hr_demand,Time>="2017-01-01 00:00:00" , Time<="2017-02-01 23:59:59")

# creat graphic for real IEDemand in Jan-Feb
ggplot(real_data,aes(x=Time,y=AvrHrDemand)) + 
  geom_line() + 
  ggtitle("Real 01.01 -- 02.01 hourly IEDemand")

hr_data <- pull(hr_test,AvrHrDemand)

# Declare Time Series Data
hr_ts <- ts(hr_data, start=1,frequency = 24)

# show the real data
autoplot(hr_ts) +
  ggtitle("Real hourly IEDemand") +
  ylab("AvrHrDemand") 


# fit arima model data
arima_fit <- auto.arima(hr_ts,d=1,D=1,stepwise = FALSE,approximation = FALSE, trace = T) # residual DS : 52.75415
print(summary(arima_fit))
checkresiduals(arima_fit)

# forecasting with arima model
# forecasating nect 24h or one day IEDemand
fcst <-  forecast(arima_fit,h=24)


autoplot(fcst) +
  autolayer(fcst,series="forecast", PI=FALSE) +
  ggtitle("Forecasts for hourly IEDemand") +
  ylab("AvrHrDemand") +
  guides(colour=guide_legend(title="arima model forecast"))

