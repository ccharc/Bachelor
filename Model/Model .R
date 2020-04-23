library(readxl)
library(vars)
library(imputeTS)
library(forecast)
library(tseries)
library(stats)
library(strucchange)

#elprice15 = data.frame(read.csv("elspot-prices_2015_hourly_eur.csv",sep=";"), "numeric")
elprice16 = data.frame(read.csv("elspot-prices_2016_hourly_eur.csv",sep=";"), "numeric")
#elprice17 = data.frame(read.csv("elspot-prices_2017_hourly_eur.csv",sep=";"), "numeric")
#elprice18 = data.frame(read.csv("elspot-prices_2018_hourly_eur.csv",sep=";"), "numeric")

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
PRICE16 = elprice16[3:8787,5]
#PRICE17 = elprice17[3:8763,4]
#PRICE18 = elprice18[3:6290,4]


SE2price = data.frame(na_interpolation(c(PRICE16), option = "linear")) 

#FORBRUG
                  
#CONS15 = cons15[554:8763,3]
CONS16 = cons16[3:8787,4]
#CONS17 = cons17[3:8763,3]
#CONS18 = cons18[3:6290,3]

SE2cons = data.frame(na_interpolation(c(CONS16), option = "linear"))


#VINDPRODUKTION

#WIND15 = wind15[554:8763,3]
WIND16 = wind16[3:8787,4]
#WIND17 = wind17[3:8763,3]
#WIND18 = wind18[3:6290,3]



SE2wind =data.frame(na_interpolation(c(WIND16), option = "linear"))

#DATO

dato1 <- seq(c(ISOdate(2016,1,1,0)), by = "hours", length.out = 8785)

#DATA

data = data.frame(dato1,SE2price,SE2cons,SE2wind)

se2pricekvart1 = data[1:2182,1:2]
se2pricekvart2 = data[2183:4366,1:2]
se2pricekvart3 = data[4367:6574,1:2]
se2pricekvart4 = data[6575:8785,1:2]

se2conskvart1 = data[1:2182,c(1,3)]
se2conskvart2 = data[2183:4366,c(1,3)]
se2conskvart3 = data[4367:6574,c(1,3)]
se2conskvart4 = data[6575:8785,c(1,3)]

se2windkvart1 = data[1:2182,c(1,4)]
se2windkvart2 = data[2183:4366,c(1,4)]
se2windkvart3 = data[4367:6574,c(1,4)]
se2windkvart4 = data[6575:8785,c(1,4)]
# model





swind = glm(data[,4] ~ time(data[,1]) + 
                I(time(data[,1])^2) +
                sin((2*pi)/365.25*I(time(data[,1]))) + 
                cos((2*pi)/365.25*I(time(data[,1])))+
                sin((4*pi)/365.25*I(time(data[,1])))+ 
                cos((4*pi)/365.25*I(time(data[,1])))+
                sin((8*pi)/365.25*I(time(data[,1])))+ 
                cos((8*pi)/365.25*I(time(data[,1])))+
                sin((24*pi)/365.25*I(time(data[,1])))+ 
                cos((24*pi)/365.25*I(time(data[,1])))+  
                sin((104*pi)/365.25*I(time(data[,1])))+ 
                cos((104*pi)/365.25*I(time(data[,1])))+
                sin((730*pi)/365.25*I(time(data[,1])))+ 
                cos((730*pi)/365.25*I(time(data[,1])))+
                sin((17520*pi)/365.25*I(time(data[,1])))+ 
                cos((17520*pi)/365.25*I(time(data[,1])))   
)
summary(swind)

x_t = ts(swind$residuals)
plot.ts(x_t)

scons= glm(data[,3] ~ time(data[,1]) + 
             I(time(data[,1])^2) +
             sin((2*pi)/365.25*I(time(data[,1]))) + 
             cos((2*pi)/365.25*I(time(data[,1])))+
             sin((4*pi)/365.25*I(time(data[,1])))+ 
             cos((4*pi)/365.25*I(time(data[,1])))+
             sin((8*pi)/365.25*I(time(data[,1])))+ 
             cos((8*pi)/365.25*I(time(data[,1])))+
             sin((24*pi)/365.25*I(time(data[,1])))+ 
             cos((24*pi)/365.25*I(time(data[,1])))+  
             sin((104*pi)/365.25*I(time(data[,1])))+ 
             cos((104*pi)/365.25*I(time(data[,1])))+
             sin((730*pi)/365.25*I(time(data[,1])))+ 
             cos((730*pi)/365.25*I(time(data[,1])))+
             sin((17520*pi)/365.25*I(time(data[,1])))+ 
             cos((17520*pi)/365.25*I(time(data[,1])))   
)
summary(scons)

z_t = ts(scons$residuals)
plot.ts(z_t)

sprice= glm(data[,2] ~ time(data[,1]) + 
             I(time(data[,1])^2) +
             sin((2*pi)/365.25*I(time(data[,1]))) + 
             cos((2*pi)/365.25*I(time(data[,1])))+
             sin((4*pi)/365.25*I(time(data[,1])))+ 
             cos((4*pi)/365.25*I(time(data[,1])))+
             sin((8*pi)/365.25*I(time(data[,1])))+ 
             cos((8*pi)/365.25*I(time(data[,1])))+
             sin((24*pi)/365.25*I(time(data[,1])))+ 
             cos((24*pi)/365.25*I(time(data[,1])))+  
             sin((104*pi)/365.25*I(time(data[,1])))+ 
             cos((104*pi)/365.25*I(time(data[,1])))+
             sin((730*pi)/365.25*I(time(data[,1])))+ 
             cos((730*pi)/365.25*I(time(data[,1])))+
             sin((17520*pi)/365.25*I(time(data[,1])))+ 
             cos((17520*pi)/365.25*I(time(data[,1])))   
)
summary(sprice)

y_t = ts(sprice$residuals)
plot.ts(y_t)

X_t = data.frame(y_t,x_t, z_t)


model2 = VARselect(X_t, lag.max = 10)
summary(model2)

model1 =VAR(X_t, ic = "AIC", lag.max = 20)
summary(model1)

roots(model1)




