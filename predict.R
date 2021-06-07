library(aimsir17)
library(astsa)
library(dplyr)
library(forecast)
library(ggplot2)
library(MLmetrics)
library(tseries)

hr_demand <- eirgrid17 %>%
  group_by(month,day,hour) %>%
  summarise(Time=first(date),
            AvrHrDemand=mean(IEDemand),
            AvrWindGen=mean(IEGeneration))

test <- filter(hr_demand,month==1,day==9)

ts_data <- pull(test,AvrHrDemand)

ts_1       <- ts(ts_data,start=1,frequency = 24)
plot.ts(ts_1)

# The sequence is not stable yet, do Log smoothing once, and then do the difference
ts_1_log <- log(ts_1)
ts_1_diff <- diff(ts_1_log, differences=1)
plot.ts(ts_1_diff)

# check acf and pacf
acf(ts_1_diff, lag.max=24)

pacf(ts_1_diff, lag.max=24)

# get the best model
auto.arima(ts_1_log,trace=T)
#from acf and pacf can got arima (1,1,1) is best model
#arima(ts_1,order=c(1,1,1))
arima_ts <- arima(ts_1,order=c(1,1,1))

f_ts <- forecast(arima_ts,h=1,level=c(99.5))
# generate forecasting plot
plot(forecast(arima_ts,h=30))

ggplot(filter(hr_demand,month==1,day %in% 9:10),aes(x=Time,y=AvrHrDemand))+geom_point()+geom_line()

ggplot(test,aes(x=Time,y=AvrWindGen))+geom_point()+geom_line()

components.ts = decompose(ts_1)
plot(components.ts)

ao = auto.arima(ts_1_log)
predict(ao,n.ahead = 10,se.fit=T)
f <- forecast(ao,h=24)
plot(f)


# predict next month total rainfall in station Mace Head
MH_weather <- observations %>%
  group_by(month,day,station) %>%
  summarise(Time=first(date),
            TotalRainfall=sum(rain),
            MaxTemp=max(temp),
            MeanWind=mean(wdsp)) %>%
  filter(station=="MACE HEAD")

weather <- filter(MH_weather,month==1)


rainMH_data <- pull(weather,TotalRainfall)
ts_MH       <- ts(rainMH_data,start=1,frequency = 30)
plot.ts(ts_MH)

# check acf and pacf
acf(ts_MH, lag.max=30)

pacf(ts_MH, lag.max=30)

# get the best model
auto.arima(ts_MH,trace=T)
#from acf and pacf can got arima (2,1,0) is best model
#arima(ts_1,order=c(2,1,0))
arima_MH <- arima(ts_MH,order=c(2,1,0))

f_MH <- forecast(arima_MH,h=30,level=c(99.5))
# generate forecasting plot
plot(forecast(arima_MH,h=30))


ggplot(filter(MH_weather,month %in% 1:2),aes(x=Time,y=TotalRainfall))+geom_point()+geom_line()
ggplot(weather,aes(x=Time,y=MaxTemp))+geom_point()+geom_line()
