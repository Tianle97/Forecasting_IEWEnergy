# merge 2 databases together, then easy to operate
total <- merge(observations, eirgrid17)
total <- total[order(total$station)]

# pick up a small data info from Sept16-Oct20 in 2017
Sept16_Oct20 <-  filter(total,date>="2017-9-16 00:00:00" , date<="2017-10-21 00:00:00")
Semp16_Oct20_avg <- Semp16_Oct20 %>% group_by(year,month,day,hour)  %>% 
  summarise( 
    meanTemp=mean(temp), 
    meanWdsp = mean(wdsp, na.rm = T), 
    meanMsl = mean(msl,na.rm = T),  
    meanRain = an(rain,na.rm = T),
    MeanIEWindGeneration=mean(IEWindGeneration,na.rm = T),
    MeanIEDemand=mean(IEDemand,na.rm = T),
    meanRhum=mean(rhum,na.rm = T),
    Time= first(date))    
# Sept16-Oct20 wind generation vs demand
ggplot(Sept16_Oct20_avg,aes(x=meanWdsp, y=MeanIEWindGeneration))+xlab("wind speed")+ylab("wind power")+
  geom_point()+geom_smooth(method="lm")+
  ggtitle("The daily average wind generation vs demand in Sept16 to Oct23")

# wind generation vs Irish wind speed in Sept_16 to Oct_20
ggplot(data=Sept16_Oct20_avg)+geom_point(mapping = aes(x=meanWdsp, y=MeanIEWindGeneration))+
  geom_smooth(mapping = aes(x=meanWdsp, y=MeanIEWindGeneration))+
  ggtitle("The Irish wind generation vs Irish wind speed in Sept_16 to Oct_20")+facet_wrap(~station)

# Wind Speed vs rhum
ggplot(data=Sept16_Oct20_avg,aes(x=meanTemp, y=meanRhum))+
  geom_point(color="blue" )+geom_smooth(mapping = aes(x=meanTemp, y=meanRhum))+
  ggtitle("The daily average Irish wind speed relative with humidity\nin Sept_16 to Oct_20")



# Forecasting methods compare
summ <- Sept16_Oct20_avg %>% group_by(station,month,day)%>% ungroup()

testMH <- filter(summ,station=="MACE HEAD")   ## %>% slice(1:20)
tsMH_data <- pull(testMH,MeanIEWindGeneration)
training_prop <- .8
ts1       <- ts(tsMH_data,start=1,end=length(tsMH_data))
trainMH_set <- window(ts1,1,floor(length(ts1)*training_prop))
testMH_set  <- window(ts1,floor(length(ts1)*training_prop)+1,length(ts1))
## naiveMH = snaive(trainMH_set, h=length(testMH_set))
## MAPE(naiveMH$mean, testMH_set) * 100

aoMH = auto.arima(trainMH_set)
predict(aoMH,n.ahead = 10,se.fit=T)
f_MH <- forecast(aoMH,h=10)
plot(f_MH)


testAY <- filter(summ,station=="ATHENRY")   ## %>% slice(1:20)
tsAY_data <- pull(testAY,meanWdsp)
training_prop <- .8
tsAY       <- ts(tsAY_data,start=1,end=length(tsAY_data))
trainAY_set <- window(tsAY,1,floor(length(tsAY)*training_prop))
testAY_set  <- window(tsAY,floor(length(tsAY)*training_prop)+1) ##,length(tsAY))
## naiveAY = naive(trainAY_set, h=length(testAY_set))
## MAPE(naiveAY$mean, testAY_set) * 100

aoAY = auto.arima(trainAY_set)
predict(aoAY,n.ahead = 10,se.fit=T)
f_AY <- forecast(aoAY,h=10)
plot(f_AY)
