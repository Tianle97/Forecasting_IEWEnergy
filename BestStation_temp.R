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
wdg_ts <- ts(windgen$MaxWindGen, start = 1, frequency = 1)

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



#############################################################################
# pick up daily weather(msl,wdsp and rainfall)
weather <- observations %>%
  group_by(station, month, day) %>%
  summarise(
    Time = first(date),
    maxWdsp = max(wdsp, na.rm = T),
    maxTemp = max(temp),
    maxMsl =  max(msl, na.rm = T)
  )

# get the all station's name
n_station <- unique(weather$station)

# create a new dataframe for storage the station name and aic.
df_aic = data.frame(matrix(ncol = 2))
# set the column name
x <- c("station", "aic")
colnames(df_aic) <- x
#debug how much station will in the loop
n = 0
for (i in n_station) {
  p_station <- filter(weather, station == i)
  # if the wdsp value is empty them jump to next station
  if (!"-Inf"  %in% p_station$maxWdsp) {
    s_ts <- ts(p_station, start = 1)
    covariates =  s_ts[,c("maxTemp","maxWdsp")]
    s_arima <- auto.arima(wdg_ts, xreg = covariates)
    s_arima$coef
    df <- data.frame("station" = i, "aic" = s_arima$aic)
    df_aic <- rbind(df_aic, df)
    n = n + 1
  }
}
# delete the first row because the 1st row is empty
df_aic = df_aic[-1, ]

# Sort the results by AIC (smallest – largest)
df_aic  <- df_aic[order(df_aic$aic), ]
# we can know the smellest aic is NEWPORT station
head(df_aic)

# merge stations dataframe and df_aic
df_map <- merge(df_aic, stations, by = "station")
# Sort the results by AIC (smallest – largest)
df_map  <- df_map[order(df_map$aic), ]



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
  geom_point(data = df_map,
             aes(x = longitude, y = latitude, size = aic),
             colour = "skyblue") +
  geom_text_repel(
    data = df_map %>% head(5),
    aes(x = longitude, y = latitude, label = station),
    size = 2.5,
    color="darkblue"
  )+ 
  ggtitle("aic in the Irish cities(temp~wind generation)")+
  theme_void() + # let the grid(x=long,y=lat) dispeared
  coord_map() # let the map show in to the spherical display

