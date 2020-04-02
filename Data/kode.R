library(readxl)
library(vars)


elprice15 = data.frame(read.csv("elspot-prices_2015_hourly_eur.csv",sep=";"), "numeric")
elprice16 = data.frame(read.csv("elspot-prices_2016_hourly_eur.csv",sep=";"), "numeric")
elprice17 = data.frame(read.csv("elspot-prices_2017_hourly_eur.csv",sep=";"), "numeric")
elprice18 = data.frame(read.csv("elspot-prices_2018_hourly_eur.csv",sep=";"), "numeric")

cons15 = data.frame(read.csv("consumption-se-areas_2015_hourly.csv",sep=";"))
cons16 = data.frame(read.csv("consumption-se-areas_2016_hourly.csv",sep=";"))
cons17 = data.frame(read.csv("consumption-se-areas_2017_hourly.csv",sep=";"))
cons18 = data.frame(read.csv("consumption-se-areas_2018_hourly.csv",sep=";"))

wind15 = data.frame(read.csv("wind-power-se_2015_hourly.csv",sep=";"))
wind16 = data.frame(read.csv("wind-power-se_2016_hourly.csv",sep=";"))
wind17 = data.frame(read.csv("wind-power-se_2017_hourly.csv",sep=";"))
wind18 = data.frame(read.csv("wind-power-se_2018_hourly.csv",sep=";"))

#DATOER

#TIMER
#HOURS15 = dfp15[747:8763,2]
#HOURS16 = dfp16[3:8787,2]
#HOURS17 = dfp17[3:8763,2]
#HOURS18 = dfp18[3:8763,2]

#Hours = c(HOURS15,HOURS16,HOURS17,HOURS18)

#PRISER
PRICE15 = elprice15[554:8763,4]
PRICE16 = elprice16[3:8787,4]
PRICE17 = elprice17[3:8763,4]
PRICE18 = elprice18[3:6290,4]


SE1price= na_mean(c(PRICE15,PRICE16,PRICE17,PRICE18))
dfprice  = data.frame(SE1price)

#FORBRUG
                  
CONS15 = cons15[554:8763,3]
CONS16 = cons16[3:8787,3]
CONS17 = cons17[3:8763,3]
CONS18 = cons18[3:6290,3]

SE1cons = na_mean(c(CONS15,CONS16,CONS17,CONS18))
dfcons = data.frame(SE1cons)


#VINDPRODUKTION

WIND15 = wind15[554:8763,3]
WIND16 = wind16[3:8787,3]
WIND17 = wind17[3:8763,3]
WIND18 = wind18[3:6290,3]


SE1wind =na_mean(c(WIND15,WIND16,WIND17,WIND18))

#DATO

#dato15 = elprice15[554:8763,1]
#dato16 = elprice16[3:8787,1]
#dato17 = elprice17[3:8763,1]
#dato18 = elprice18[3:6290,1]

#dato = c(dato15,dato16,dato17,dato18)

#DATA

data = data.frame(SE1price,SE1cons,SE1wind)

windseries = as.ts(SE1wind)
consseries = as.ts(SE1cons)
priceseries= as.ts(SE1price)

plot.ts(priceseries)
plot.ts(windseries)
plot.ts(consseries)

acf(consseries)
acf(windseries)
acf(priceseries)










