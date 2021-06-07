##############################################
#To start off we can clear all the variables
#from the current environment
#and close all the plots.
##############################################

rm(list = ls())
graphics.off()
library("magrittr")
library("aimsir17")
library("astsa")
library("dplyr")
library("forecast")
library("ggplot2")
library("MLmetrics")
library("tseries")

# use some methods forecasting IEDemand,
demand <- eirgrid17 %>%
  group_by(month,day) %>%
  summarise(Time=first(date),
            AvrHrDemand=mean(IEDemand),
            AvrWindGen=mean(IEGeneration))

# pick up one month data for training
test <- filter(demand,month %in% 1:8)
# creat graphic for real IEDemand in Jan
ggplot(test,aes(x=Time,y=AvrHrDemand))+geom_line()

ts_data <- ts(pull(demand,AvrHrDemand),start=1,end = length(pull(demand,AvrHrDemand)),frequency = 1)
data_train <- pull(test,AvrHrDemand)

autoplot(ts_data) +
  ggtitle("real hourly Irish Energy Demand ") +
  xlab("Time") + ylab("AvrHrDemand")

#data has a strong trend. Investigate transformation.
#Take the first difference of the data to remove the trend
d <- diff(ts_data)
autoplot(d) +
  ggtitle("change in real hourly Irish Energy Demand ") +
  xlab("Time") + ylab("AvrHrDemand")


training_data <- window(ts_data, start = 1, end = length(data_train))

test_data <- window(ts_data, start = length(data_train)+1)

naive <- naive(training_data,h=length(test_data))
MAPE(naive$mean, test_data) * 100
#checkresiduals(naive)

autoplot(ts_data) +
  autolayer(naive$mean,series="Naïve") +
  ggtitle("Forecasts for hourly Irish Energy Demand ") +
  xlab("Time") + ylab("AvrHrDemand") +
  guides(colour=guide_legend(title="Forecast"))

################################
#naive is not good ideal model forecasting with us 
################################



# we try ETS Method
ets_m <-  ets(training_data)
print(summary(ets_m))
checkresiduals(ets_m)
ets_f <- forecast(ets_m,h=length(test_data))
MAPE(ets_f$mean, test_data) *100
ets_f$mean
autoplot(ts_data) +
  autolayer(ets_f$mean,series="ETS", PI=FALSE) +
  ggtitle("Forecasts for hourly Irish Energy Demand ") +
  xlab("Time") + ylab("AvrHrDemand") +
  guides(colour=guide_legend(title="Forecast"))




