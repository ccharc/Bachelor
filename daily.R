library(readxl)
library(vars)
library(imputeTS)
library(forecast)
library(tseries)
library(stats)
library(strucchange)
library(tidyr)
library(astsa)
library(car)
library(Matrix)


polymult <- function(x,a,b)
  # a(B) = a_0 + a_1*B + a_2*B² + ... + a_p*B^p
  # b(B) = b_0 + b_1*B + b_2*B² + ... + a_q*B^q
  # where wlog p >= q
  # Result is a(B)b(B) x_t
{
  if (length(b) == 1){polycoeff <- b[1]*array(a)}
  else
  {alpha <- array(a)
  p <- dim(alpha) - 1
  beta <- array(b)
  q <- dim(beta) - 1
  # Assumed: p >= q
  polycoeff <- array(rep(0,p+q + 1))
  for (k in 0:(p+q))
    for (j in max(0,k-q):min(k,p))
    {polycoeff[k+1] <- polycoeff[k+1] + alpha[j+1]*beta[k-j+1]}
  }
  return(filter(x,c(polycoeff),sides=1, method="convolution"))
  # test: return(polycoeff)
}


elprice17 =  data.frame(read.csv("elspot-prices_2017_daily_eur.csv",sep=";",dec=","))

cons17= data.frame(read.csv("consumption-se-areas_2017_daily.csv",sep=";"))

prod17 = data.frame(read.csv("production-se-areas_2017_daily.csv",sep=";"))


Price = log(na_interpolation(as.numeric(elprice17[3:367,4]), option = "linear"))

Cons = log(na_interpolation(as.numeric(as.character(cons17[3:367,3])), option = "linear"))

Prod = log(na_interpolation(as.numeric(as.character(prod17[3:367,3])), option = "linear"))


plot.ts(Price)

plot.ts(Cons)

plot.ts(Prod)

acf(Price)

acf(Cons)

acf(Prod)



dato <- seq(c(ISOdate(2017,1,1)), by = "days", length.out = 365)

data1 = data.frame(dato, Price, Cons, Prod)

Prod1= data1[,c(1,4)]

Cons1 = data1[,c(1,3)]

Price1 = data1[,c(1,2)]

sprod = glm(Prod1[,2] ~ time(Prod1[,1]) + 
                    I(time(Prod1[,1])^2) +
                    sin((2*pi)/365*I(time(Prod1[,1])))+ 
                    cos((2*pi)/365*I(time(Prod1[,1])))+
                    sin((4*pi)/365*I(time(Prod1[,1])))+ 
                    cos((4*pi)/365*I(time(Prod1[,1])))+  
                    sin((8*pi)/365*I(time(Prod1[,1])))+ 
                    cos((8*pi)/365*I(time(Prod1[,1])))+
                    sin((24*pi)/365*I(time(Prod1[,1])))+ 
                    cos((24*pi)/365*I(time(Prod1[,1])))+
                    sin((104*pi)/365*I(time(Prod1[,1])))+ 
                    cos((104*pi)/365*I(time(Prod1[,1])))+
                    sin((730*pi)/365*I(time(Prod1[,1])))+ 
                    cos((730*pi)/365*I(time(Prod1[,1])))
)

sprice = glm(Price1[,2] ~ time(Price1[,1]) + 
              I(time(Price1[,1])^2) +
              sin((2*pi)/365*I(time(Price1[,1])))+ 
              cos((2*pi)/365*I(time(Price1[,1])))+
              sin((4*pi)/365*I(time(Price1[,1])))+ 
              cos((4*pi)/365*I(time(Price1[,1])))+  
              sin((8*pi)/365*I(time(Price1[,1])))+ 
              cos((8*pi)/365*I(time(Price1[,1])))+
              sin((24*pi)/365*I(time(Price1[,1])))+ 
              cos((24*pi)/365*I(time(Price1[,1])))+
              sin((104*pi)/365*I(time(Price1[,1])))+ 
              cos((104*pi)/365*I(time(Price1[,1])))+
              sin((730*pi)/365*I(time(Price1[,1])))+ 
              cos((730*pi)/365*I(time(Price1[,1])))
)


scons = glm(Cons1[,2] ~ time(Cons1[,1]) + 
              I(time(Cons1[,1])^2) +
              sin((2*pi)/365*I(time(Cons1[,1])))+ 
              cos((2*pi)/365*I(time(Cons1[,1])))+
              sin((4*pi)/365*I(time(Cons1[,1])))+ 
              cos((4*pi)/365*I(time(Cons1[,1])))+  
              sin((8*pi)/365*I(time(Cons1[,1])))+ 
              cos((8*pi)/365*I(time(Cons1[,1])))+
              sin((24*pi)/365*I(time(Cons1[,1])))+ 
              cos((24*pi)/365*I(time(Cons1[,1])))+
              sin((104*pi)/365*I(time(Cons1[,1])))+ 
              cos((104*pi)/365*I(time(Cons1[,1])))+
              sin((730*pi)/365*I(time(Cons1[,1])))+ 
              cos((730*pi)/365*I(time(Cons1[,1])))
)

summary(sprod)
summary(scons)
summary(sprice)


sprod_res = as.ts(sprod$residuals)
sprice_res = as.ts(sprice$residuals)
scons_res = as.ts(scons$residuals)

acf2(sprod_res)

acf2(diff(sprod_res,1))

acf2(diff(sprod_res,24))

acf2(diff(diff(sprod_res,1),24))

seas_sprod_res = sarima(sprod_res,3,0,0,2,0,0,7, no.constant = TRUE)

seas_sprod_res$AIC #-3.553287

seas_x1res= ts(seas_sprod_res$fit$residuals)

acf2(seas_x1res)

adf.test(seas_x1res) # stationær

coefs_x1 <- seas_sprod_res$fit$coef
print(coefs_x1)

coefs_saeson_Phi_x1 <- c(1,rep(0,5),-coefs_x1[4:5])
coefs_saeson_Phi_x1

# (1-Phi*B^24)*(1-B)x_1t = stationær AR(3) proces :
stat_x1 <- polymult(sprod_res,coefs_saeson_Phi_x1,1)
acf2(stat_x1)

qqPlot(seas_sprod_res$fit$residuals, ylab = "sprod residualer", xlab = "Norm Quantiles")

acf(seas_sprod_res$fit$residuals)

LBQPlot(seas_sprod_res$fit$residuals)






acf2(sprice_res)

seas_sprice_res = sarima(sprice_res,3,0,0,1,0,0,7, no.constant = TRUE)

seas_sprice_res$AIC #0.1526423

seas_y1res = ts(seas_sprice_res$fit$residuals)

acf2(seas_y1res)

adf.test(seas_y1res) # stationær

coefs_y1 <- seas_sprice_res$fit$coef
print(coefs_y1)

coefs_saeson_Phi_y1 <- c(1,rep(0,6),-coefs_y1[4])
coefs_saeson_Phi_y1

# (1-Phi*B^24)*(1-B)x_1t = stationær AR(3) proces :
stat_y1 <- polymult(sprice_res,coefs_saeson_Phi_y1,1)
acf2(stat_y1)

qqPlot(seas_sprice_res$fit$residuals, ylab = "sprice residualer", xlab = "Norm Quantiles")

acf(seas_y1res)

LBQPlot(seas_y1res)



acf2(scons_res)

seas_scons_res = sarima(scons_res,1,0,0,1,0,0,7, no.constant = TRUE)

seas_scons_res$AIC #-4.965997

seas_z1res = ts(seas_scons_res$fit$residuals)

acf2(seas_z1res)

adf.test(seas_z1res) # stationær

coefs_z1 <- seas_scons_res$fit$coef
print(coefs_z1)

coefs_saeson_Phi_z1 <- c(1,rep(0,6),-coefs_z1[2])
coefs_saeson_Phi_z1

# (1-Phi*B^24)*(1-B)x_1t = stationær AR(3) proces :
stat_z1 <- polymult(scons_res,coefs_saeson_Phi_z1,1)
acf2(stat_z1)


qqPlot(seas_scons_res$fit$residuals, ylab = "scons residualer", xlab = "Norm Quantiles")

acf(seas_z1res)

LBQPlot(seas_z1res)


plot(stat_y1)

plot(stat_x1)

plot(stat_z1)


XXX = data.frame(stat_z1,stat_x1,stat_y1)[8:365,]

var_xx = VAR(XXX, lag.max = 8)

VARselect(XXX)

summary(var_xx)


var = restrict(var_xx, method = "ser")

summary(var)

roots(var)
stability(var)

causality(var, cause = "stat_y1")
causality(var, cause = "stat_x1")
causality(var, cause = "stat_z1")



irf = irf(var, ortho = TRUE)

plot(irf)


serial.test(var, type = "BG") #Der er ikke seriekorrelation
arch.test(var) #  Fejlledene er  homoskedastiske
normality.test(var) #  Residualerne er ikke normalfordelte



amat = diag(3)
diag(amat) = NA
amat[2,1] = NA
amat[3,1] = NA
amat[3,2] = NA

amat
 
bmat = diag(3)
diag(bmat) = NA


svar = SVAR(var, Amat= amat, estmethod = "direct")

svar$Sigma.U
svar$A

svar$A %*% svar$Sigma.U %*% t(svar$A)

summary(svar)


irf_svar = irf(svar)
plot(irf_svar)
