

se2pricemnd = data[1:743,1:2]

se2consmnd = data[1:743,c(1,3)]

se2windmnd = data[1:743,c(1,4)]


spricemnd = glm(se2pricemnd[,2] ~ time(se2pricemnd[,1]) + 
                  I(time(se2pricemnd[,1])^2) +
                  sin((24*pi)/365.25*I(time(se2pricemnd[,1])))+ 
                  cos((24*pi)/365.25*I(time(se2pricemnd[,1])))+  
                  sin((104*pi)/365.25*I(time(se2pricemnd[,1])))+ 
                  cos((104*pi)/365.25*I(time(se2pricemnd[,1])))+
                  sin((730*pi)/365.25*I(time(se2pricemnd[,1])))+ 
                  cos((730*pi)/365.25*I(time(se2pricemnd[,1])))+
                  sin((17520*pi)/365.25*I(time(se2pricemnd[,1])))+ 
                  cos((17520*pi)/365.25*I(time(se2pricemnd[,1])))   
)

swindmnd = glm(se2windmnd[,2] ~ time(se2windmnd[,1]) + 
                 I(time(se2windmnd[,1])^2) +
                 sin((24*pi)/365.25*I(time(se2windmnd[,1])))+ 
                 cos((24*pi)/365.25*I(time(se2windmnd[,1])))+  
                 sin((104*pi)/365.25*I(time(se2windmnd[,1])))+ 
                 cos((104*pi)/365.25*I(time(se2windmnd[,1])))+
                 sin((730*pi)/365.25*I(time(se2windmnd[,1])))+ 
                 cos((730*pi)/365.25*I(time(se2windmnd[,1])))+
                 sin((17520*pi)/365.25*I(time(se2windmnd[,1])))+ 
                 cos((17520*pi)/365.25*I(time(se2windmnd[,1])))   
)

sconsmnd = glm(se2consmnd[,2] ~ time(se2consmnd[,1]) + 
                 I(time(se2consmnd[,1])^2) +
                 sin((8*pi)/365.25*I(time(se2consmnd[,1])))+ 
                 cos((8*pi)/365.25*I(time(se2consmnd[,1])))+
                 sin((24*pi)/365.25*I(time(se2consmnd[,1])))+ 
                 cos((24*pi)/365.25*I(time(se2consmnd[,1])))+  
                 sin((104*pi)/365.25*I(time(se2consmnd[,1])))+ 
                 cos((104*pi)/365.25*I(time(se2consmnd[,1])))+
                 sin((730*pi)/365.25*I(time(se2consmnd[,1])))+ 
                 cos((730*pi)/365.25*I(time(se2consmnd[,1])))+
                 sin((17520*pi)/365.25*I(time(se2consmnd[,1])))+ 
                 cos((17520*pi)/365.25*I(time(se2consmnd[,1])))   
)

yyt = ts(spricemnd$residuals)

xxt = ts(swindmnd$residuals)

zzt = ts(sconsmnd$residuals)

XXT = data.frame(yyt,xxt, zzt)


FIT1 = VAR(XXT, ic = "AIC", lag.max = 10)

summary(FIT1)