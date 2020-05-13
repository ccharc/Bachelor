### Sæson kvartal price

se2pricekvart1 = data[1:2183,1:2]
se2pricekvart2 = data[2184:4367,1:2]
se2pricekvart3 = data[4368:6575,1:2]
se2pricekvart4 = data[6576:8784,1:2]


### Sæson kvartal cons

se2conskvart1 = data[1:2183,c(1,3)]
se2conskvart2 = data[2184:4367,c(1,3)]
se2conskvart3 = data[4368:6575,c(1,3)]
se2conskvart4 = data[6576:8784,c(1,3)]


### Sæson kvartal wind

se2prodkvart1 = data[1:2183,c(1,4)]
se2prodkvart2 = data[2184:4367,c(1,4)]
se2prodkvart3 = data[4368:6575,c(1,4)]
se2prodkvart4 = data[6576:8784,c(1,4)]

sprodkvart1 = glm(se2prodkvart1[,2] ~ time(se2prodkvart1[,1]) + 
                     I(time(se2prodkvart1[,1])^2) +
                     sin((8*pi)/8784*I(time(se2prodkvart1[,1])))+ 
                     cos((8*pi)/8784*I(time(se2prodkvart1[,1])))+
                     sin((24*pi)/8784*I(time(se2prodkvart1[,1])))+ 
                     cos((24*pi)/8784*I(time(se2prodkvart1[,1])))+  
                     sin((104*pi)/8784*I(time(se2prodkvart1[,1])))+ 
                     cos((104*pi)/8784*I(time(se2prodkvart1[,1])))+
                     sin((732*pi)/8784*I(time(se2prodkvart1[,1])))+ 
                     cos((732*pi)/8784*I(time(se2prodkvart1[,1])))+
                     sin((1464*pi)/8784*I(time(se2prodkvart1[,1])))+ 
                     cos((1464*pi)/8784*I(time(se2prodkvart1[,1])))+
                     sin((17568*pi)/8784*I(time(se2prodkvart1[,1])))+ 
                     cos((17568*pi)/8784*I(time(se2prodkvart1[,1])))
)

sprodkvart2 = glm(se2prodkvart2[,2] ~ time(se2prodkvart2[,1]) + 
                    I(time(se2prodkvart2[,1])^2) +
                    sin((8*pi)/8784*I(time(se2prodkvart2[,1])))+ 
                    cos((8*pi)/8784*I(time(se2prodkvart2[,1])))+
                    sin((24*pi)/8784*I(time(se2prodkvart2[,1])))+ 
                    cos((24*pi)/8784*I(time(se2prodkvart2[,1])))+  
                    sin((104*pi)/8784*I(time(se2prodkvart2[,1])))+ 
                    cos((104*pi)/8784*I(time(se2prodkvart2[,1])))+
                    sin((732*pi)/8784*I(time(se2prodkvart2[,1])))+ 
                    cos((732*pi)/8784*I(time(se2prodkvart2[,1])))+
                    sin((1464*pi)/8784*I(time(se2prodkvart2[,1])))+ 
                    cos((1464*pi)/8784*I(time(se2prodkvart2[,1])))+
                    sin((17568*pi)/8784*I(time(se2prodkvart2[,1])))+ 
                    cos((17568*pi)/8784*I(time(se2prodkvart2[,1])))   
)

sprodkvart3 = glm(se2prodkvart3[,2] ~ time(se2prodkvart3[,1]) + 
                    I(time(se2prodkvart3[,1])^2) +
                    sin((8*pi)/8784*I(time(se2prodkvart3[,1])))+ 
                    cos((8*pi)/8784*I(time(se2prodkvart3[,1])))+
                    sin((24*pi)/8784*I(time(se2prodkvart3[,1])))+ 
                    cos((24*pi)/8784*I(time(se2prodkvart3[,1])))+  
                    sin((104*pi)/8784*I(time(se2prodkvart3[,1])))+ 
                    cos((104*pi)/8784*I(time(se2prodkvart3[,1])))+
                    sin((732*pi)/8784*I(time(se2prodkvart3[,1])))+ 
                    cos((732*pi)/8784*I(time(se2prodkvart3[,1])))+
                    sin((1464*pi)/8784*I(time(se2prodkvart3[,1])))+ 
                    cos((1464*pi)/8784*I(time(se2prodkvart3[,1])))+
                    sin((17568*pi)/8784*I(time(se2prodkvart3[,1])))+ 
                    cos((17568*pi)/8784*I(time(se2prodkvart3[,1])))   
)
  
sprodkvart4 = glm(se2prodkvart4[,2] ~ time(se2prodkvart4[,1]) + 
                    I(time(se2prodkvart4[,1])^2) +
                    sin((8*pi)/8784*I(time(se2prodkvart4[,1])))+ 
                    cos((8*pi)/8784*I(time(se2prodkvart4[,1])))+
                    sin((24*pi)/8784*I(time(se2prodkvart4[,1])))+ 
                    cos((24*pi)/8784*I(time(se2prodkvart4[,1])))+  
                    sin((104*pi)/8784*I(time(se2prodkvart4[,1])))+ 
                    cos((104*pi)/8784*I(time(se2prodkvart4[,1])))+
                    sin((732*pi)/8784*I(time(se2prodkvart4[,1])))+ 
                    cos((732*pi)/8784*I(time(se2prodkvart4[,1])))+
                    sin((1464*pi)/8784*I(time(se2prodkvart4[,1])))+ 
                    cos((1464*pi)/8784*I(time(se2prodkvart4[,1])))+
                    sin((17568*pi)/8784*I(time(se2prodkvart4[,1])))+ 
                    cos((17568*pi)/8784*I(time(se2prodkvart4[,1])))   
)


### Sæson kvartal consumption

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
                      sin((1464*pi)/8784*I(time(se2conskvart1[,1])))+ 
                      cos((1464*pi)/8784*I(time(se2conskvart1[,1])))+
                      sin((17568*pi)/8784*I(time(se2conskvart1[,1])))+ 
                      cos((17568*pi)/8784*I(time(se2conskvart1[,1])))   
)

sconskvart2 = glm(se2conskvart2[,2] ~ time(se2conskvart2[,1]) + 
                    I(time(se2conskvart2[,1])^2) +
                    sin((8*pi)/8784*I(time(se2conskvart2[,1])))+ 
                    cos((8*pi)/8784*I(time(se2conskvart2[,1])))+
                    sin((24*pi)/8784*I(time(se2conskvart2[,1])))+ 
                    cos((24*pi)/8784*I(time(se2conskvart2[,1])))+  
                    sin((104*pi)/8784*I(time(se2conskvart2[,1])))+ 
                    cos((104*pi)/8784*I(time(se2conskvart2[,1])))+
                    sin((732*pi)/8784*I(time(se2conskvart2[,1])))+ 
                    cos((732*pi)/8784*I(time(se2conskvart2[,1])))+
                    sin((1464*pi)/8784*I(time(se2conskvart2[,1])))+ 
                    cos((1464*pi)/8784*I(time(se2conskvart2[,1])))+
                    sin((17568*pi)/8784*I(time(se2conskvart2[,1])))+ 
                    cos((17568*pi)/8784*I(time(se2conskvart2[,1])))   
)

sconskvart3 = glm(se2conskvart3[,2] ~ time(se2conskvart3[,1]) + 
                    I(time(se2conskvart3[,1])^2) +
                    sin((8*pi)/8784*I(time(se2conskvart3[,1])))+ 
                    cos((8*pi)/8784*I(time(se2conskvart3[,1])))+
                    sin((24*pi)/8784*I(time(se2conskvart3[,1])))+ 
                    cos((24*pi)/8784*I(time(se2conskvart3[,1])))+  
                    sin((104*pi)/8784*I(time(se2conskvart3[,1])))+ 
                    cos((104*pi)/8784*I(time(se2conskvart3[,1])))+
                    sin((732*pi)/8784*I(time(se2conskvart3[,1])))+ 
                    cos((732*pi)/8784*I(time(se2conskvart3[,1])))+
                    sin((1464*pi)/8784*I(time(se2conskvart3[,1])))+ 
                    cos((1464*pi)/8784*I(time(se2conskvart3[,1])))+
                    sin((17568*pi)/8784*I(time(se2conskvart3[,1])))+ 
                    cos((17568*pi)/8784*I(time(se2conskvart3[,1])))   
)

sconskvart4 = glm(se2conskvart4[,2] ~ time(se2conskvart4[,1]) + 
                    I(time(se2conskvart4[,1])^2) +
                    sin((8*pi)/8784*I(time(se2conskvart4[,1])))+ 
                    cos((8*pi)/8784*I(time(se2conskvart4[,1])))+
                    sin((24*pi)/8784*I(time(se2conskvart4[,1])))+ 
                    cos((24*pi)/8784*I(time(se2conskvart4[,1])))+  
                    sin((104*pi)/8784*I(time(se2conskvart4[,1])))+ 
                    cos((104*pi)/8784*I(time(se2conskvart4[,1])))+
                    sin((732*pi)/8784*I(time(se2conskvart4[,1])))+ 
                    cos((732*pi)/8784*I(time(se2conskvart4[,1])))+
                    sin((1464*pi)/8784*I(time(se2conskvart4[,1])))+ 
                    cos((1464*pi)/8784*I(time(se2conskvart4[,1])))+
                    sin((17568*pi)/8784*I(time(se2conskvart4[,1])))+ 
                    cos((17568*pi)/8784*I(time(se2conskvart4[,1])))   
)




### Sæson kvartal price

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
                    sin((1464*pi)/8784*I(time(se2pricekvart1[,1])))+ 
                    cos((1464*pi)/8784*I(time(se2pricekvart1[,1])))+
                    sin((17568*pi)/8784*I(time(se2pricekvart1[,1])))+ 
                    cos((17568*pi)/8784*I(time(se2pricekvart1[,1])))   
)

spricekvart2 = glm(se2pricekvart2[,2] ~ time(se2pricekvart2[,1]) + 
                     I(time(se2pricekvart2[,1])^2) +
                     sin((8*pi)/8784*I(time(se2pricekvart2[,1])))+ 
                     cos((8*pi)/8784*I(time(se2pricekvart2[,1])))+
                     sin((24*pi)/8784*I(time(se2pricekvart2[,1])))+ 
                     cos((24*pi)/8784*I(time(se2pricekvart2[,1])))+  
                     sin((104*pi)/8784*I(time(se2pricekvart2[,1])))+ 
                     cos((104*pi)/8784*I(time(se2pricekvart2[,1])))+
                     sin((732*pi)/8784*I(time(se2pricekvart2[,1])))+ 
                     cos((732*pi)/8784*I(time(se2pricekvart2[,1])))+
                     sin((1464*pi)/8784*I(time(se2pricekvart2[,1])))+ 
                     cos((1464*pi)/8784*I(time(se2pricekvart2[,1])))+
                     sin((17568*pi)/8784*I(time(se2pricekvart2[,1])))+ 
                     cos((17568*pi)/8784*I(time(se2pricekvart2[,1])))   
)

spricekvart3 = glm(se2pricekvart3[,2] ~ time(se2pricekvart3[,1]) + 
                     I(time(se2pricekvart3[,1])^2) +
                     sin((8*pi)/8784*I(time(se2pricekvart3[,1])))+ 
                     cos((8*pi)/8784*I(time(se2pricekvart3[,1])))+
                     sin((24*pi)/8784*I(time(se2pricekvart3[,1])))+ 
                     cos((24*pi)/8784*I(time(se2pricekvart3[,1])))+  
                     sin((104*pi)/8784*I(time(se2pricekvart3[,1])))+ 
                     cos((104*pi)/8784*I(time(se2pricekvart3[,1])))+
                     sin((732*pi)/8784*I(time(se2pricekvart3[,1])))+ 
                     cos((732*pi)/8784*I(time(se2pricekvart3[,1])))+
                     sin((1464*pi)/8784*I(time(se2pricekvart3[,1])))+ 
                     cos((1464*pi)/8784*I(time(se2pricekvart3[,1])))+
                     sin((17568*pi)/8784*I(time(se2pricekvart3[,1])))+ 
                     cos((17568*pi)/8784*I(time(se2pricekvart3[,1])))   
)

spricekvart4 = glm(se2pricekvart4[,2] ~ time(se2pricekvart4[,1]) + 
                     I(time(se2pricekvart4[,1])^2) +
                     sin((8*pi)/8784*I(time(se2pricekvart4[,1])))+ 
                     cos((8*pi)/8784*I(time(se2pricekvart4[,1])))+
                     sin((24*pi)/8784*I(time(se2pricekvart4[,1])))+ 
                     cos((24*pi)/8784*I(time(se2pricekvart4[,1])))+  
                     sin((104*pi)/8784*I(time(se2pricekvart4[,1])))+ 
                     cos((104*pi)/8784*I(time(se2pricekvart4[,1])))+
                     sin((732*pi)/8784*I(time(se2pricekvart4[,1])))+ 
                     cos((732*pi)/8784*I(time(se2pricekvart4[,1])))+
                     sin((1464*pi)/8784*I(time(se2pricekvart4[,1])))+ 
                     cos((1464*pi)/8784*I(time(se2pricekvart4[,1])))+
                     sin((17568*pi)/8784*I(time(se2pricekvart4[,1])))+ 
                     cos((17568*pi)/8784*I(time(se2pricekvart4[,1])))   
)

# VAR Modeller kvart

# PROD
x_t1 = as.ts(sprodkvart1$residuals)[1:2182]

x_t2 = ts(sprodkvart2$residuals)

x_t3 = ts(sprodkvart3$residuals)

x_t4 = ts(sprodkvart4$residuals)

plot.ts(x_t1)

plot.ts(x_t2)

plot.ts(x_t3)

plot.ts(x_t4)


#CONS

z_t1 = ts(sconskvart1$residuals)[1:2182]

z_t2 = ts(sconskvart2$residuals)

z_t3 = ts(sconskvart3$residuals)

z_t4 = ts(sconskvart4$residuals)

plot.ts(z_t1)

plot.ts(z_t2)

plot.ts(z_t3)

plot.ts(z_t4)

# PRICE

y_t1 = ts(spricekvart1$residuals)
  
y_t2 = ts(spricekvart2$residuals)

y_t3 = ts(spricekvart3$residuals)

y_t4 = ts(spricekvart4$residuals)

plot.ts(y_t1)

# VAR MODEL

X_t1 = data.frame(y_t1, x_t1, z_t1)

X_t2 = data.frame(y_t2,x_t2, z_t2)

X_t3 = data.frame(y_t3,x_t3, z_t3)

X_t4 = data.frame(y_t4,x_t4, z_t4)


fit1 = VAR(X_t1, ic = "AIC", lag.max = 24)

fit2 = VAR(X_t2, ic = "AIC", lag.max = 24)

fit3 = VAR(X_t3, ic = "AIC", lag.max = 24)

fit4 = VAR(X_t4, ic = "AIC", lag.max = 24)




summary(fit1)

summary(fit2)

summary(fit3)

summary(fit4)

