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
library("astsa")
######################################################

# use Arima model to forecasting hourly IEWind Generation

######################################################


windgen <- eirgrid17 %>%
  group_by(year, month, day) %>%
  summarise(
    Time = first(date),
    AvrDemand = mean(IEDemand),
    MaxWindGen = max(IEGeneration)
  )


# creat graphic for real daily maximum Irish wind Generation
ggplot(windgen, aes(x = Time, y = MaxWindGen)) +
  geom_line() +
  ggtitle("Irish Wind Generation daily maximum in 2017")

# Declare Time Series Data
wdg_ts <- ts(windgen$MaxWindGen, start = 1,frequency = 1)

# show the real data
autoplot(wdg_ts) +
  ggtitle("Real daily max IEGeneration") +
  ylab("MaxWindGen")


# fit arima model data
arima_wdg <- auto.arima(wdg_ts)
arima_wdg #aic : 5087.91
print(arima_wdg$aic)
print(summary(arima_wdg))
checkresiduals(arima_wdg)

#arima_fit <- Arima(wdg_ts, order = c(2,1,1), method = "ML")

#accuracy(arima_fit)
#qqnorm(arima_fit$residuals)
#qqline(arima_fit$residuals)
#Box.test(arima_fit$residuals,type="Ljung-Box")


# forecasting with arima model
# forecasating nect 24h or one day IEDemand
fcst <-  forecast(arima_wdg, 30)
fcst

# show the forecast graphic
autoplot(wdg_ts) +
  autolayer(fcst, series = "forecast", PI = FALSE) +
  ggtitle("Forecasts for daily Irish wind generation") +
  ylab("MaxWindGen") +
  guides(colour = guide_legend(title = "arima model forecast"))



# pick up daily weather(msl,wdsp and rainfall)
MH_weather <- observations %>%
  group_by(month,day) %>%
  summarise(Time=first(date),
            maxWdsp=max(wdsp,na.rm = T),
            station = "MACE HEAD",
            MaxTemp=max(temp))


MH_ts <- ts(MH_weather,start=1)

arima_MH <- auto.arima(wdg_ts,xreg = MH_ts[,"maxWdsp"])
arima_MH # AIC = 5036.92

fcast <- forecast(arima_MH, xreg = MH_ts[, "maxWdsp"])
fcast

autoplot(wdg_ts) +
  autolayer(fcast, series = "forecast", PI = FALSE) +
  ggtitle("Forecasts for daily (MACE HEAD)Irish wind generation") +
  ylab("MaxWindGen") +
  guides(colour = guide_legend(title = "arima model forecast"))




