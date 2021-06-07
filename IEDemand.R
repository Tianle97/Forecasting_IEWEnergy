# maximum wind energy generated IEWindGeneration & maximum daily windspeed wdsp
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
library("maps")
library("mapdata")
library("ggmap")
library("ggrepel")  
######################################################

# use Arima model to forecasting hourly IE Demand

######################################################


IED <- eirgrid17 %>%
  group_by(year, month, day) %>%
  summarise(
    Time = first(date),
    AvrDemand = mean(IEDemand),
    MaxWindGen = max(IEGeneration)
  )


# creat graphic for real daily maximum Irish wind Generation
ggplot(IED, aes(x = Time, y = AvrDemand)) +
  geom_line() +
  ggtitle("Average daily Irish Energy Demand in 2017")

# Declare Time Series Data
ied_ts <- ts(IED$AvrDemand, start = 1, frequency = 1)

# show the real data
autoplot(ied_ts) +
  ggtitle("Real daily average IEDemand") +
  ylab("Average Irish Energy Demand")


# fit arima model data
arima_ied <- auto.arima(ied_ts)
arima_ied #aic : 4627.43
print(arima_ied$aic)
print(summary(arima_ied))
checkresiduals(arima_ied)

#arima_fit <- Arima(wdg_ts, order = c(2,1,1), method = "ML")

#accuracy(arima_fit)
#qqnorm(arima_fit$residuals)
#qqline(arima_fit$residuals)
#Box.test(arima_fit$residuals,type="Ljung-Box")


# forecasting with arima model
# forecasating nect 24h or one day IEDemand
fcst <-  forecast(arima_ied, 30)
fcst

# show the forecast graphic
autoplot(ied_ts) +
  autolayer(fcst, series = "forecast", PI = FALSE) +
  ggtitle("Forecasts for daily Irish wind generation") +
  ylab("Average Irish Energy Demand") +
  guides(colour = guide_legend(title = "arima model forecast"))



#############################################################################
# pick up daily weather(msl,wdsp and rainfall)
weather <- observations %>%
  group_by(station, month, day) %>%
  summarise(
    Time = first(date),
    maxWdsp = max(wdsp, na.rm = T),
    maxTemp = max(temp),
    maxRain = max(rain),
    maxMsl = max(msl),
    maxRhum = max(rhum)
  )

# get the all station's name
n_station <- unique(weather$station)

# create a new dataframe for storage the station name and aic.
dfIE_aic = data.frame(matrix(ncol = 2))
# set the column name
x <- c("station", "aic")
colnames(dfIE_aic) <- x
#debug how much station will in the loop
n = 0
for (i in n_station) {
  p_station <- filter(weather, station == i)
  # if the wdsp value is empty them jump to next station
  if (!"-Inf"  %in% p_station$maxWdsp) {
    s_ts <- ts(p_station, start = 1)
    covariates =  s_ts[, c("maxWdsp","maxTemp","maxRain","maxRhum","maxMsl")]
    s_arima <- auto.arima(ied_ts, xreg = covariates)
    df <- data.frame("station" = i, "aic" = s_arima$aic)
    dfIE_aic <- rbind(dfIE_aic, df)
    n = n + 1
  }
}
# delete the first row because the 1st row is empty
dfIE_aic = dfIE_aic[-1, ]

# Sort the results by AIC (smallest – largest)
dfIE_aic  <- dfIE_aic[order(dfIE_aic$aic), ]
# we can know the smellest aic is NEWPORT station
head(dfIE_aic)

# merge stations dataframe and dfIE_aic
IED_map <- merge(dfIE_aic, stations, by = "station")
# Sort the results by AIC (smallest – largest)
IED_map  <- IED_map[order(IED_map$aic), ]



##########################################
# show the informations in the map
##########################################

# get all details from ireland cities
Ire <- map_data("world") %>% filter(region=="Ireland")
data <- world.cities %>% filter(country.etc=="Ireland")


ggplot() + geom_polygon(
  data = Ire,
  aes(x = long, y = lat, group = group),
  fill = "grey",
  colour="yellow",
  size=0.15,
  alpha = 0.3
) +
  geom_point(data = IED_map,
             aes(x = longitude, y = latitude, size = aic),
             colour = "skyblue") +
  geom_text_repel(
    data = IED_map %>% head(5),
    aes(x = longitude, y = latitude, label = station),
    size = 2.5,
    color="darkblue"
  )+ 
  ggtitle("aic in the Irish cities")+
  theme_void() + # let the grid(x=long,y=lat) dispeared
  coord_map() # let the map show in to the spherical display

