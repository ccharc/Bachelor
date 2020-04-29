library(readxl)
library(vars)
library(imputeTS)
library(forecast)
library(tseries)
library(stats)
library(strucchange)
library(tidyr)

#elprice15 = data.frame(read.csv("elspot-prices_2015_hourly_eur.csv",sep=";"))
elprice16 =  data.frame(read.csv("elspot-prices_2016_hourly_eur.csv",sep=";",dec=","))
#elprice17 = data.frame(read.csv("elspot-prices_2017_hourly_eur.csv",sep=";"))
#elprice18 = data.frame(read.csv("elspot-prices_2018_hourly_eur.csv",sep=";"))

#cons15 = data.frame(read.csv("consumption-se-areas_2015_hourly.csv",sep=";"))
cons16 = data.frame(read.csv("consumption-se-areas_2016_hourly.csv",sep=";"))
#cons17 = data.frame(read.csv("consumption-se-areas_2017_hourly.csv",sep=";"))
#cons18 = data.frame(read.csv("consumption-se-areas_2018_hourly.csv",sep=";"))

#wind15 = data.frame(read.csv("wind-power-se_2015_hourly.csv",sep=";"))
wind16 = data.frame(read.csv("wind-power-se_2016_hourly.csv",sep=";"))
#wind17 = data.frame(read.csv("wind-power-se_2017_hourly.csv",sep=";"))
#wind18 = data.frame(read.csv("wind-power-se_2018_hourly.csv",sep=";"))


#PRISER
#PRICE15 = elprice15[554:8763,4]
PRICE16 = elprice16[1:8784,5]
#PRICE17 = elprice17[3:8763,4]
#PRICE18 = elprice18[3:6290,4]


SE2price = data.frame(na_interpolation(c(PRICE16), option = "linear"))



#FORBRUG
                  
#CONS15 = cons15[554:8763,3]
CONS16 = cons16[3:8786,4]
#CONS17 = cons17[3:8763,3]
#CONS18 = cons18[3:6290,3]

SE2cons = data.frame(na_interpolation(c(CONS16), option = "linear"))



#VINDPRODUKTION

#WIND15 = wind15[554:8763,3]
WIND16 = wind16[3:8786,4]
#WIND17 = wind17[3:8763,3]
#WIND18 = wind18[3:6290,3]

SE2wind = data.frame(na_interpolation(c(WIND16), option = "linear"))

#DATO

dato1 <- seq(c(ISOdate(2016,1,1,0)), by = "hours", length.out = 8784)

#DATA

data = data.frame(dato1,SE2price,SE2cons,SE2wind)

plot.ts(PRICE16)

plot.ts(CONS16)

plot.ts(WIND16)


