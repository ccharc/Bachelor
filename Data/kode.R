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



dfp13 = data.frame(elprice13)
dfp14 = data.frame(elprice14)
dfp15 = data.frame(elprice15)
dfp16 = data.frame(elprice16)
dfp17 = data.frame(elprice17)
dfp18 = data.frame(elprice18)
dfp19 = data.frame(elprice19)
dfp20 = data.frame(elprice20)

PRICE13 = dfp13[3:8763,4]
PRICE14 = dfp14[3:8763,4]
PRICE15 = dfp15[3:8763,4]
PRICE16 = dfp16[3:8787,4]
PRICE17 = dfp17[3:8763,4]
PRICE18 = dfp18[3:8763,4]
PRICE19 = dfp19[3:8763,4]
PRICE20 = dfp20[3:1658,4]

HOURS13 = dfp13[3:8763,2]
HOURS14 = dfp14[3:8763,2]
HOURS15 = dfp15[3:8763,2]
HOURS16 = dfp16[3:8787,2]
HOURS17 = dfp17[3:8763,2]
HOURS18 = dfp18[3:8763,2]
HOURS19 = dfp19[3:8763,2]
HOURS20 = dfp20[3:1658,2]

Hours = c(HOURS15,HOURS16,HOURS17,HOURS18,HOURS19,HOURS20)
SE1price= c(PRICE15,PRICE16,PRICE17,PRICE18,PRICE19,PRICE20)

ELspot = data.frame(Hours,SE1price)
                  
dfc13 = data.frame(cons13)
dfc14 = data.frame(cons14)
dfc15 = data.frame(cons15)
dfc16 = data.frame(cons16)
dfc17 = data.frame(cons17)
dfc18 = data.frame(cons18)
dfc19 = data.frame(cons19)
dfc20 = data.frame(cons20)

CONS13 = dfc13[3:8763,3]
CONS14 = dfc14[3:8763,3]
CONS15 = dfc15[3:8763,3]
CONS16 = dfc16[3:8787,3]
CONS17 = dfc17[3:8763,3]
CONS18 = dfc18[3:8763,3]
CONS19 = dfc19[3:8763,3]
CONS20 = dfc20[3:1658,3]

SE1cons = c(CONS15,CONS16,CONS17,CONS18,CONS19,CONS20)

Cons = data.frame(Hours,SE1cons)

Wind = data.frame(windprod13_20[746:46224,3])


timerwind = data.frame(windprod13_20[746:46224,1])




