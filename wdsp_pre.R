library(aimsir17)
library(astsa)
library(dplyr)
library(forecast)
library(ggplot2)
library(MLmetrics)
library(tseries)

# pick up the west of Ireland County
west <- filter(stations, county %in% c("Galway","Mayo","Roscommon"))
# pick the stations
west_stations <- west$station

# pick up daily weather(msl,wdsp and rainfall)
daily_weather <- observations %>%
  group_by(station,month,day) %>%
  summarise(Time=first(date),
            AvrWdsp=mean(wdsp,na.rm = T),
            TotalRainfall=sum(rain,na.rm = T),
            AvrMsl=mean(msl,na.rm = T),
            MaxTemp=max(temp))

# pick up west stations
train_data <- filter(daily_weather,station %in% "ATHENRY") %>% slice(1:100)

# creat graphic for real wdsp in west stations
#ggplot(train_data,aes(x=AvrMsl,y=AvrWdsp))+geom_point()+facet_wrap(~station)

training_prop <- .8

data <- pull(train_data,AvrWdsp)

ts_weather <- ts(data,start=1,end=length(data))
plot(ts_weather)
training_data <- window(ts_weather, start = 1, end = (length(ts_weather)*training_prop))
test_data <- window(ts_weather, start = floor(length(training_data)+1),end=length(ts_weather))

# naive method 
naive <- snaive(training_data, h=length(test_data))
print(summary(naive))
MAPE(naive$mean, test_data) * 100

#generate graph
autoplot(ts_weather) +
  autolayer(naive$mean,series="Naïve", PI=FALSE) +
  ggtitle("Forecasts for wind speed") +
  ylab("AvrWdsp") +
  guides(colour=guide_legend(title="Forecast"))


arima_optimal <- auto.arima(training_data)

ao_f <- forecast(arima_optimal, h= length(test_data))

plot(ao_f)

autoplot(ts_weather) +
  autolayer(ao_f,series="arimar model", PI=FALSE) +
  ggtitle("Forecasts for wind speed") +
  xlab("Time") + ylab("AvrWdsp") +
  guides(colour=guide_legend(title="Forecast"))

checkresiduals(arima_optimal)


