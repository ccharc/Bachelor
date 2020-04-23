### Sæson kvartal wind

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

swindkvart1 = glm(se2windkvart1[,2] ~ time(se2windkvart1[,1]) + 
                     I(time(se2windkvart1[,1])^2) +
                     sin((8*pi)/365.25*I(time(se2windkvart1[,1])))+ 
                     cos((8*pi)/365.25*I(time(se2windkvart1[,1])))+
                     sin((24*pi)/365.25*I(time(se2windkvart1[,1])))+ 
                     cos((24*pi)/365.25*I(time(se2windkvart1[,1])))+  
                     sin((104*pi)/365.25*I(time(se2windkvart1[,1])))+ 
                     cos((104*pi)/365.25*I(time(se2windkvart1[,1])))+
                     sin((730*pi)/365.25*I(time(se2windkvart1[,1])))+ 
                     cos((730*pi)/365.25*I(time(se2windkvart1[,1])))+
                     sin((17520*pi)/365.25*I(time(se2windkvart1[,1])))+ 
                     cos((17520*pi)/365.25*I(time(se2windkvart1[,1])))   
)

swindkvart2 = glm(se2windkvart2[,2] ~ time(se2windkvart2[,1]) + 
                    I(time(se2windkvart2[,1])^2) +
                    sin((8*pi)/365.25*I(time(se2windkvart2[,1])))+ 
                    cos((8*pi)/365.25*I(time(se2windkvart2[,1])))+
                    sin((24*pi)/365.25*I(time(se2windkvart2[,1])))+ 
                    cos((24*pi)/365.25*I(time(se2windkvart2[,1])))+  
                    sin((104*pi)/365.25*I(time(se2windkvart2[,1])))+ 
                    cos((104*pi)/365.25*I(time(se2windkvart2[,1])))+
                    sin((730*pi)/365.25*I(time(se2windkvart2[,1])))+ 
                    cos((730*pi)/365.25*I(time(se2windkvart2[,1])))+
                    sin((17520*pi)/365.25*I(time(se2windkvart2[,1])))+ 
                    cos((17520*pi)/365.25*I(time(se2windkvart2[,1])))   
)

swindkvart3 = glm(se2windkvart3[,2] ~ time(se2windkvart3[,1]) + 
                    I(time(se2windkvart3[,1])^2) +
                    sin((8*pi)/365.25*I(time(se2windkvart3[,1])))+ 
                    cos((8*pi)/365.25*I(time(se2windkvart3[,1])))+
                    sin((24*pi)/365.25*I(time(se2windkvart3[,1])))+ 
                    cos((24*pi)/365.25*I(time(se2windkvart3[,1])))+  
                    sin((104*pi)/365.25*I(time(se2windkvart3[,1])))+ 
                    cos((104*pi)/365.25*I(time(se2windkvart3[,1])))+
                    sin((730*pi)/365.25*I(time(se2windkvart3[,1])))+ 
                    cos((730*pi)/365.25*I(time(se2windkvart3[,1])))+
                    sin((17520*pi)/365.25*I(time(se2windkvart3[,1])))+ 
                    cos((17520*pi)/365.25*I(time(se2windkvart3[,1])))   
)
  
swindkvart4 = glm(se2windkvart4[,2] ~ time(se2windkvart4[,1]) + 
                    I(time(se2windkvart4[,1])^2) +
                    sin((8*pi)/365.25*I(time(se2windkvart4[,1])))+ 
                    cos((8*pi)/365.25*I(time(se2windkvart4[,1])))+
                    sin((24*pi)/365.25*I(time(se2windkvart4[,1])))+ 
                    cos((24*pi)/365.25*I(time(se2windkvart4[,1])))+  
                    sin((104*pi)/365.25*I(time(se2windkvart4[,1])))+ 
                    cos((104*pi)/365.25*I(time(se2windkvart4[,1])))+
                    sin((730*pi)/365.25*I(time(se2windkvart4[,1])))+ 
                    cos((730*pi)/365.25*I(time(se2windkvart4[,1])))+
                    sin((17520*pi)/365.25*I(time(se2windkvart4[,1])))+ 
                    cos((17520*pi)/365.25*I(time(se2windkvart4[,1])))   
)

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


### Sæson kvartal consumption

sconskvart1 = glm(se2conskvart1[,2] ~ time(se2conskvart1[,1]) + 
                      I(time(se2conskvart1[,1])^2) +
                      sin((8*pi)/365.25*I(time(se2conskvart1[,1])))+ 
                      cos((8*pi)/365.25*I(time(se2conskvart1[,1])))+
                      sin((24*pi)/365.25*I(time(se2conskvart1[,1])))+ 
                      cos((24*pi)/365.25*I(time(se2conskvart1[,1])))+  
                      sin((104*pi)/365.25*I(time(se2conskvart1[,1])))+ 
                      cos((104*pi)/365.25*I(time(se2conskvart1[,1])))+
                      sin((730*pi)/365.25*I(time(se2conskvart1[,1])))+ 
                      cos((730*pi)/365.25*I(time(se2conskvart1[,1])))+
                      sin((17520*pi)/365.25*I(time(se2conskvart1[,1])))+ 
                      cos((17520*pi)/365.25*I(time(se2conskvart1[,1])))   
)

sconskvart2 = glm(se2conskvart2[,2] ~ time(se2conskvart2[,1]) + 
                    I(time(se2conskvart2[,1])^2) +
                    sin((8*pi)/365.25*I(time(se2conskvart2[,1])))+ 
                    cos((8*pi)/365.25*I(time(se2conskvart2[,1])))+
                    sin((24*pi)/365.25*I(time(se2conskvart2[,1])))+ 
                    cos((24*pi)/365.25*I(time(se2conskvart2[,1])))+  
                    sin((104*pi)/365.25*I(time(se2conskvart2[,1])))+ 
                    cos((104*pi)/365.25*I(time(se2conskvart2[,1])))+
                    sin((730*pi)/365.25*I(time(se2conskvart2[,1])))+ 
                    cos((730*pi)/365.25*I(time(se2conskvart2[,1])))+
                    sin((17520*pi)/365.25*I(time(se2conskvart2[,1])))+ 
                    cos((17520*pi)/365.25*I(time(se2conskvart2[,1])))   
)

sconskvart3 = glm(se2conskvart3[,2] ~ time(se2conskvart3[,1]) + 
                    I(time(se2conskvart3[,1])^2) +
                    sin((8*pi)/365.25*I(time(se2conskvart3[,1])))+ 
                    cos((8*pi)/365.25*I(time(se2conskvart3[,1])))+
                    sin((24*pi)/365.25*I(time(se2conskvart3[,1])))+ 
                    cos((24*pi)/365.25*I(time(se2conskvart3[,1])))+  
                    sin((104*pi)/365.25*I(time(se2conskvart3[,1])))+ 
                    cos((104*pi)/365.25*I(time(se2conskvart3[,1])))+
                    sin((730*pi)/365.25*I(time(se2conskvart3[,1])))+ 
                    cos((730*pi)/365.25*I(time(se2conskvart3[,1])))+
                    sin((17520*pi)/365.25*I(time(se2conskvart3[,1])))+ 
                    cos((17520*pi)/365.25*I(time(se2conskvart3[,1])))   
)

sconskvart4 = glm(se2conskvart4[,2] ~ time(se2conskvart4[,1]) + 
                    I(time(se2conskvart4[,1])^2) +
                    sin((8*pi)/365.25*I(time(se2conskvart4[,1])))+ 
                    cos((8*pi)/365.25*I(time(se2conskvart4[,1])))+
                    sin((24*pi)/365.25*I(time(se2conskvart4[,1])))+ 
                    cos((24*pi)/365.25*I(time(se2conskvart4[,1])))+  
                    sin((104*pi)/365.25*I(time(se2conskvart4[,1])))+ 
                    cos((104*pi)/365.25*I(time(se2conskvart4[,1])))+
                    sin((730*pi)/365.25*I(time(se2conskvart4[,1])))+ 
                    cos((730*pi)/365.25*I(time(se2conskvart4[,1])))+
                    sin((17520*pi)/365.25*I(time(se2conskvart4[,1])))+ 
                    cos((17520*pi)/365.25*I(time(se2conskvart4[,1])))   
)


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


### Sæson kvartal price

spricekvart1 = glm(se2pricekvart1[,2] ~ time(se2pricekvart1[,1]) + 
                    I(time(se2pricekvart1[,1])^2) +
                    sin((8*pi)/365.25*I(time(se2pricekvart1[,1])))+ 
                    cos((8*pi)/365.25*I(time(se2pricekvart1[,1])))+
                    sin((24*pi)/365.25*I(time(se2pricekvart1[,1])))+ 
                    cos((24*pi)/365.25*I(time(se2pricekvart1[,1])))+  
                    sin((104*pi)/365.25*I(time(se2pricekvart1[,1])))+ 
                    cos((104*pi)/365.25*I(time(se2pricekvart1[,1])))+
                    sin((730*pi)/365.25*I(time(se2pricekvart1[,1])))+ 
                    cos((730*pi)/365.25*I(time(se2pricekvart1[,1])))+
                    sin((17520*pi)/365.25*I(time(se2pricekvart1[,1])))+ 
                    cos((17520*pi)/365.25*I(time(se2pricekvart1[,1])))   
)

spricekvart2 = glm(se2pricekvart2[,2] ~ time(se2pricekvart2[,1]) + 
                     I(time(se2pricekvart2[,1])^2) +
                     sin((8*pi)/365.25*I(time(se2pricekvart2[,1])))+ 
                     cos((8*pi)/365.25*I(time(se2pricekvart2[,1])))+
                     sin((24*pi)/365.25*I(time(se2pricekvart2[,1])))+ 
                     cos((24*pi)/365.25*I(time(se2pricekvart2[,1])))+  
                     sin((104*pi)/365.25*I(time(se2pricekvart2[,1])))+ 
                     cos((104*pi)/365.25*I(time(se2pricekvart2[,1])))+
                     sin((730*pi)/365.25*I(time(se2pricekvart2[,1])))+ 
                     cos((730*pi)/365.25*I(time(se2pricekvart2[,1])))+
                     sin((17520*pi)/365.25*I(time(se2pricekvart2[,1])))+ 
                     cos((17520*pi)/365.25*I(time(se2pricekvart2[,1])))   
)

spricekvart3 = glm(se2pricekvart3[,2] ~ time(se2pricekvart3[,1]) + 
                     I(time(se2pricekvart3[,1])^2) +
                     sin((8*pi)/365.25*I(time(se2pricekvart3[,1])))+ 
                     cos((8*pi)/365.25*I(time(se2pricekvart3[,1])))+
                     sin((24*pi)/365.25*I(time(se2pricekvart3[,1])))+ 
                     cos((24*pi)/365.25*I(time(se2pricekvart3[,1])))+  
                     sin((104*pi)/365.25*I(time(se2pricekvart3[,1])))+ 
                     cos((104*pi)/365.25*I(time(se2pricekvart3[,1])))+
                     sin((730*pi)/365.25*I(time(se2pricekvart3[,1])))+ 
                     cos((730*pi)/365.25*I(time(se2pricekvart3[,1])))+
                     sin((17520*pi)/365.25*I(time(se2pricekvart3[,1])))+ 
                     cos((17520*pi)/365.25*I(time(se2pricekvart3[,1])))   
)

spricekvart4 = glm(se2pricekvart4[,2] ~ time(se2pricekvart4[,1]) + 
                     I(time(se2pricekvart4[,1])^2) +
                     sin((8*pi)/365.25*I(time(se2pricekvart4[,1])))+ 
                     cos((8*pi)/365.25*I(time(se2pricekvart4[,1])))+
                     sin((24*pi)/365.25*I(time(se2pricekvart4[,1])))+ 
                     cos((24*pi)/365.25*I(time(se2pricekvart4[,1])))+  
                     sin((104*pi)/365.25*I(time(se2pricekvart4[,1])))+ 
                     cos((104*pi)/365.25*I(time(se2pricekvart4[,1])))+
                     sin((730*pi)/365.25*I(time(se2pricekvart4[,1])))+ 
                     cos((730*pi)/365.25*I(time(se2pricekvart4[,1])))+
                     sin((17520*pi)/365.25*I(time(se2pricekvart4[,1])))+ 
                     cos((17520*pi)/365.25*I(time(se2pricekvart4[,1])))   
)


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

model11 =VAR(X_t, ic = "AIC", lag.max = 20)
summary(model11)

# VAR Modeller kvart

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


# VAR MODEL

X_t1 = data.frame(y_t1,x_t1, z_t1)

X_t2 = data.frame(y_t2,x_t2, z_t2)

X_t3 = data.frame(y_t3,x_t3, z_t3)

X_t4 = data.frame(y_t4,x_t4, z_t4)


fit1 = VAR(X_t1, ic = "AIC", lag.max = 10)

fit2 = VAR(X_t2, ic = "AIC", lag.max = 10)

fit3 = VAR(X_t3, ic = "AIC", lag.max = 10)

fit4 = VAR(X_t4, ic = "AIC", lag.max = 10)

summary(fit1)

summary(fit2)

summary(fit3)

summary(fit4)




