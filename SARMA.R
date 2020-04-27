### SARMA 

### Sæson kvartal price

se2pricekvart1 = data[1:2182,1:2]
se2pricekvart2 = data[2183:4366,1:2]
se2pricekvart3 = data[4367:6574,1:2]
se2pricekvart4 = data[6575:8784,1:2]


### Sæson kvartal cons

se2conskvart1 = data[1:2182,c(1,3)]
se2conskvart2 = data[2183:4366,c(1,3)]
se2conskvart3 = data[4367:6574,c(1,3)]
se2conskvart4 = data[6575:8784,c(1,3)]


### Sæson kvartal wind

se2windkvart1 = data[1:2182,c(1,4)]
se2windkvart2 = data[2183:4366,c(1,4)]
se2windkvart3 = data[4367:6574,c(1,4)]
se2windkvart4 = data[6575:8784,c(1,4)]
swindkvart1 = glm(se2windkvart1[,2] ~ time(se2windkvart1[,1]) + 
                    I(time(se2windkvart1[,1])^2) +
                    sin((8*pi)/8784*I(time(se2windkvart1[,1])))+ 
                    cos((8*pi)/8784*I(time(se2windkvart1[,1])))+
                    sin((24*pi)/8784*I(time(se2windkvart1[,1])))+ 
                    cos((24*pi)/8784*I(time(se2windkvart1[,1])))+  
                    sin((104*pi)/8784*I(time(se2windkvart1[,1])))+ 
                    cos((104*pi)/8784*I(time(se2windkvart1[,1])))+
                    sin((732*pi)/8784*I(time(se2windkvart1[,1])))+ 
                    cos((732*pi)/8784*I(time(se2windkvart1[,1])))+
                    sin((17570*pi)/8784*I(time(se2windkvart1[,1])))+ 
                    cos((17570*pi)/8784*I(time(se2windkvart1[,1])))   
)

sconskvart1 = glm(se2conskvart1[,2] ~ time(se2conskvart1[,1]) + 
                    I(time(se2conskvart1[,1])^2) +
                    sin((8*pi)/8784*I(time(se2conskvart1[,1])))+ 
                    cos((8*pi)/8784*I(time(se2conskvart1[,1])))+
                    sin((24*pi)/8784*I(time(se2conskvart1[,1])))+ 
                    cos((24*pi)/8784*I(time(se2conskvart1[,1])))+  
                    sin((104*pi)/8784*I(time(se2conskvart1[,1])))+ 
                    cos((104*pi)/8784*I(time(se2conskvart1[,1])))+
                    sin((732*pi)/8784*I(time(se2conskvart1[,1])))+ 
                    cos((732*pi)/8784*I(time(se2conskvart1[,1])))+
                    sin((17570*pi)/8784*I(time(se2conskvart1[,1])))+ 
                    cos((17570*pi)/8784*I(time(se2conskvart1[,1])))   
)

spricekvart1 = glm(se2pricekvart1[,2] ~ time(se2pricekvart1[,1]) + 
                     I(time(se2pricekvart1[,1])^2) +
                     sin((8*pi)/8784*I(time(se2pricekvart1[,1])))+ 
                     cos((8*pi)/8784*I(time(se2pricekvart1[,1])))+
                     sin((24*pi)/8784*I(time(se2pricekvart1[,1])))+ 
                     cos((24*pi)/8784*I(time(se2pricekvart1[,1])))+  
                     sin((104*pi)/8784*I(time(se2pricekvart1[,1])))+ 
                     cos((104*pi)/8784*I(time(se2pricekvart1[,1])))+
                     sin((732*pi)/8784*I(time(se2pricekvart1[,1])))+ 
                     cos((732*pi)/8784*I(time(se2pricekvart1[,1])))+
                     sin((17570*pi)/8784*I(time(se2pricekvart1[,1])))+ 
                     cos((17570*pi)/8784*I(time(se2pricekvart1[,1])))   
)
# WIND
x_t1 = ts(swindkvart1$residuals)

x_t2 = ts(swindkvart2$residuals)

x_t3 = ts(swindkvart3$residuals)

x_t4 = ts(swindkvart4$residuals)

#CONS

z_t1 = ts(sconskvart1$residuals)

z_t2 = ts(sconskvart2$residuals)

z_t3 = ts(sconskvart3$residuals)

z_t4 = ts(sconskvart4$residuals)

# PRICE

y_t1 = ts(spricekvart1$residuals)

y_t2 = ts(spricekvart2$residuals)

y_t3 = ts(spricekvart3$residuals)

y_t4 = ts(spricekvart4$residuals)








acf(x_t1)

seas_x1 = auto.arima(x_t1, d=0, seasonal = TRUE)

res_x1 = ts(seas_x1$residuals)

seas_y1 = auto.arima(y_t1, d=0, seasonal = TRUE)

res_y1 = ts(seas_y1$residuals)

seas_z1 = auto.arima(z_t1, d=0, seasonal = TRUE)

res_z1 = ts(seas_z1$residuals)

seas_Xt = data.frame(res_y1, res_x1, res_z1)

seas_fit1 = VAR(seas_Xt, ic = "AIC", lag.max = 10)
summary(seas_fit1)
