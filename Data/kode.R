library(readxl)
elprice13 = read_xls("elspot-prices_2013_hourly_eur.xls")
elprice14 = read_xls("elspot-prices_2014_hourly_eur.xls")
elprice15 = read_xls("elspot-prices_2015_hourly_eur.xls")
elprice16 = read_xls("elspot-prices_2016_hourly_eur.xls")
elprice17 = read_xls("elspot-prices_2017_hourly_eur.xls")
elprice18 = read_xls("elspot-prices_2018_hourly_eur.xls")
elprice19 = read_xls("elspot-prices_2019_hourly_eur.xls")
elprice20 = read_xls("elspot-prices_2020_hourly_eur.xls")

cons13 = read_xls("consumption-se-areas_2013_hourly.xls")
cons14 = read_xls("consumption-se-areas_2014_hourly.xls")
cons15 = read_xls("consumption-se-areas_2015_hourly.xls")
cons16 = read_xls("consumption-se-areas_2016_hourly.xls")
cons17 = read_xls("consumption-se-areas_2017_hourly.xls")
cons18 = read_xls("consumption-se-areas_2018_hourly.xls")
cons19 = read_xls("consumption-se-areas_2019_hourly.xls")
cons20 = read_xls("consumption-se-areas_2020_hourly.xls")

wind15 = read_xls("wind-power-se_2015_hourly.xls")
wind16 = read_xls("wind-power-se_2016_hourly.xls")
wind17 = read_xls("wind-power-se_2017_hourly.xls")
wind18 = read_xls("wind-power-se_2018_hourly.xls")
wind19 = read_xls("wind-power-se_2019_hourly.xls")
wind20 = read_xls("wind-power-se_2020_hourly.xls")

dates15 = seq(as.Date("2015-01-01"),as.Date("2015-12-31"),1)
dates16 = seq(as.Date("2016-01-01"),as.Date("2016-12-31"),1)
dates17 = seq(as.Date("2017-01-01"),as.Date("2017-12-31"),1)
dates18 = seq(as.Date("2018-01-01"),as.Date("2018-12-31"),1)

dates = c(dates15,dates16,dates17,dates18)

hours <- merge(1:24, seq(0, 45, by = 60))


dfp15 = data.frame(elprice15)
dfp16 = data.frame(elprice16)
dfp17 = data.frame(elprice17)
dfp18 = data.frame(elprice18)
dfp19 = data.frame(elprice19)
dfp20 = data.frame(elprice20)


PRICE15 = dfp15[556:8763,4]
PRICE16 = dfp16[3:8787,4]
PRICE17 = dfp17[3:8763,4]
PRICE18 = dfp18[3:8763,4]
PRICE19 = dfp19[3:8763,4]
PRICE20 = dfp20[3:1658,4]


HOURS15 = dfp15[556:8763,2]
HOURS16 = dfp16[3:8787,2]
HOURS17 = dfp17[3:8763,2]
HOURS18 = dfp18[3:8763,2]
HOURS19 = dfp19[3:8763,2]
HOURS20 = dfp20[3:1658,2]

Hours = c(HOURS15,HOURS16,HOURS17,HOURS18)

ELspot = data.frame(Hours,SE1price)
                  
dfc15 = data.frame(cons15)
dfc16 = data.frame(cons16)
dfc17 = data.frame(cons17)
dfc18 = data.frame(cons18)
dfc19 = data.frame(cons19)
dfc20 = data.frame(cons20)

CONS15 = dfc15[556:8763,3]
CONS16 = dfc16[3:8787,3]
CONS17 = dfc17[3:8763,3]
CONS18 = dfc18[3:8763,3]
CONS19 = dfc19[3:8763,3]
CONS20 = dfc20[3:1658,3]

Cons = data.frame(Hours,SE1cons)

dfw15 = data.frame(wind15)
dfw16 = data.frame(wind16)
dfw17 = data.frame(wind17)
dfw18 = data.frame(wind18)
dfw19 = data.frame(wind19)
dfw20 = data.frame(wind20)

WIND15 = dfw15[556:8763,3]
WIND16 = dfw16[3:8787,3]
WIND17 = dfw17[3:8763,3]
WIND18 = dfw18[3:8763,3]
WIND19 = dfw19[3:8763,3]
WIND20 = dfw20[3:1658,3]

SE1wind = c(WIND15,WIND16,WIND17,WIND18)

SE1cons = c(CONS15,CONS16,CONS17,CONS18)

SE1price= c(PRICE15,PRICE16,PRICE17,PRICE18)

data = data.frame(Hours,SE1price,SE1cons,SE1wind)

windseries = ts(SE1wind)
consseries = ts(SE1cons)
priceseries= ts(SE1price)

plot.ts(priceseries)
