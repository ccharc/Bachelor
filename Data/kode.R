library(readxl)

elprice15 = read_xls("elspot-prices_2015_hourly_eur.xls")
elprice16 = read_xls("elspot-prices_2016_hourly_eur.xls")
elprice17 = read_xls("elspot-prices_2017_hourly_eur.xls")
elprice18 = read_xls("elspot-prices_2018_hourly_eur.xls")

cons15 = read_xls("consumption-se-areas_2015_hourly.xls")
cons16 = read_xls("consumption-se-areas_2016_hourly.xls")
cons17 = read_xls("consumption-se-areas_2017_hourly.xls")
cons18 = read_xls("consumption-se-areas_2018_hourly.xls")

wind15 = read_xls("wind-power-se_2015_hourly.xls")
wind16 = read_xls("wind-power-se_2016_hourly.xls")
wind17 = read_xls("wind-power-se_2017_hourly.xls")
wind18 = read_xls("wind-power-se_2018_hourly.xls")

#DATOER
#values15 = seq(from = as.Date("2015-02-01"), to = as.Date("2015-12-31"), by = 'day')
#values16 = seq(from = as.Date("2016-01-01"), to = as.Date("2016-12-31"), by = 'day')
#values17 = seq(from = as.Date("2017-01-01"), to = as.Date("2017-12-31"), by = 'day')
#values18 = seq(from = as.Date("2018-01-01"), to = as.Date("2018-12-31"), by = 'day')

#date15 = rep(values15, each = 24)
#date16 = rep(values16, each = 24)
#date17 = rep(values17, each = 24)
#date18 = rep(values18, each = 24)

#dates = c(date15,date16,date17,date18)


#TIMER
#HOURS15 = dfp15[747:8763,2]
#HOURS16 = dfp16[3:8787,2]
#HOURS17 = dfp17[3:8763,2]
#HOURS18 = dfp18[3:8763,2]

#Hours = c(HOURS15,HOURS16,HOURS17,HOURS18)

#PRISER
dfp15 = data.frame(elprice15)
dfp16 = data.frame(elprice16)
dfp17 = data.frame(elprice17)
dfp18 = data.frame(elprice18)

PRICE15 = dfp15[747:8763,4]
PRICE16 = dfp16[3:8787,4]
PRICE17 = dfp17[3:8763,4]
PRICE18 = dfp18[3:8763,4]


SE1price= c(PRICE15,PRICE16,PRICE17,PRICE18)


#FORBRUG
                  
dfc15 = data.frame(cons15)
dfc16 = data.frame(cons16)
dfc17 = data.frame(cons17)
dfc18 = data.frame(cons18)


CONS15 = dfc15[747:8763,3]
CONS16 = dfc16[3:8787,3]
CONS17 = dfc17[3:8763,3]
CONS18 = dfc18[3:8763,3]

SE1cons = c(CONS15,CONS16,CONS17,CONS18)


#VINDPRODUKTION
dfw15 = data.frame(wind15)
dfw16 = data.frame(wind16)
dfw17 = data.frame(wind17)
dfw18 = data.frame(wind18)


WIND15 = dfw15[747:8763,3]
WIND16 = dfw16[3:8787,3]
WIND17 = dfw17[3:8763,3]
WIND18 = dfw18[3:8763,3]


SE1wind = c(WIND15,WIND16,WIND17,WIND18)

#DATA

data = data.frame(SE1price,SE1cons,SE1wind)

windseries = ts(SE1wind)
consseries = ts(SE1cons)
priceseries= ts(SE1price)

plot.ts(priceseries)
plot.ts(windseries)
plot.ts(consseries)

library(vars)





