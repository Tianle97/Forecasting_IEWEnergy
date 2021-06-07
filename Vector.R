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
library("urca")
library("vars")
library("mFilter")

# pick up the west of Ireland County
west <- filter(stations, county %in% c("Galway","Mayo","Roscommon"))
# pick the stations
west_stations <- west$station

daily_weather <- observations %>%
  group_by(station,month,day) %>%
  summarise(Time=first(date),
            AvrWdsp=mean(wdsp,na.rm = T),
            TotalRainfall=sum(rain,na.rm = T),
            AvrMsl=mean(msl,na.rm = T),
            MaxTemp=max(temp))


ggplot(data = filter(daily_weather,station == west_stations,month == 1)) + geom_point(mapping = aes(x=Time,y=AvrWdsp)) + facet_wrap(~station) 


ggplot(data = filter(daily_weather,station == "MACE HEAD",month == 1)) + geom_point(mapping = aes(x=Time,y=AvrWdsp)) + facet_wrap(~station) 

#Declare Time Series Variable
ts_wdsp <- ts((filter(daily_weather,station=="MACE HEAD"))$AvrWdsp, start=1,frequency = 1)
ts_temp <- ts((filter(daily_weather,station=="MACE HEAD"))$MaxTemp, start=1,frequency = 1)

autoplot(cbind(ts_wdsp,ts_temp))

#OLS（ordinary least square） 最小二乘回归法
ols <- lm(ts_wdsp ~ ts_temp)
summary(ols)
# we can see 0.19625 for temp so temp is negtive for the wind speed



# determine the Persistence of the model

acf(ts_wdsp,mian = "acf for real wind speed ")
pacf(ts_wdsp,mian="pacf for real wind speed")

acf(ts_temp,mian="acf for real temp ")
pacf(ts_temp,mian="pacf for real temp ")


# find the optimal lag

data  <- cbind(ts_wdsp,ts_temp)
colnames(data) <- cbind("wdsp","temp")

lagselect <- VARselect(data,lag.max = 10,type = "const")
lagselect$selection


#build VAR

var1 <- VAR(data, p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")
var2 <- VAR(data, p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")

var3 <- VAR(data, p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")
summary(var3)

forecast(var3) %>%
  autoplot() + xlab("time") 


#varModel <- VAR(data,p=6,type="const",season = NULL,exog=NULL)












