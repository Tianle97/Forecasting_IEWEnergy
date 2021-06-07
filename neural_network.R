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
library("zoo")


daily_demand <- eirgrid17 %>%
  group_by(month, day) %>%
  summarise(
    Time = first(date),
    AvrDemand = mean(IEDemand),
    AvrWindGen = mean(IEGeneration)
  )

ts_demand <-  ts(daily_demand$AvrDemand,
                 start = 1,
                 frequency = 1)

autoplot(ts_demand)

# neural network

model  <- nnetar(ts_demand)
fcst <-  forecast(model, h = 30)

fcst %>%
  autoplot() + xlab("time")


# simulation of 9 possible future sample paths for demand data
#  each sample path cover the next 30days
sim <- ts(matrix(0, nrow = 30L, ncol = 9L),
          start = end(ts_demand)[1L] + 1L)
for (i in seq(9))
  sim[, i] <- simulate(model, nsim = 30L)
autoplot(ts_demand) + autolayer(sim) +
  xlab("time") + ylab("demand") +
  theme(text = element_text(family = "STHeiti"))


fcast <- forecast(model, PI = TRUE, h = 30)
autoplot(fcast) +
  xlab("time") + ylab("demand")
