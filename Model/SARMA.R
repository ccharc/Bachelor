### SARMA 

### Sæson xt

acf2(x_t1)

acf2(diff(x_t1,1))

acf2(diff(x_t1,24))

acf2(diff(diff(x_t1,1),24))

seas_x1 = sarima(x_t1,3,0,0,2,0,0,24, no.constant = TRUE)

seas_x1$AIC

seas_x1res= ts(seas_x1$fit$residuals)

acf2(seas_x1res)

adf.test(seas_x1res) # stationær

coefs_x1 <- seas_x1$fit$coef
print(coefs_x1)

coefs_saeson_Phi_x1 <- c(1,rep(0,23),-coefs_x1[2])
coefs_saeson_Phi_x1

# (1-Phi*B^24)*(1-B)x_1t = stationær AR(3) proces :
stat_x1 <- polymult(diff(x_t1),coefs_saeson_Phi_x1,1)
acf2(stat_x1)





acf2(x_t2) 

acf2(diff(x_t2,1))

acf2(diff(x_t2,24))

acf2(diff(diff(x_t2,1),24))

seas_x2 = sarima(x_t2,1,1,0,1,1,0,24, no.constant = TRUE)

seas_x2res = ts(seas_x2$fit$residuals)

acf2(seas_x2res)

adf.test(seas_x2res) # stationær

coefs_x2 <- seas_x2$fit$coef
print(coefs_x2)

coefs_saeson_Phi_x2 <- c(1,rep(0,23),-coefs_x2[2])
coefs_saeson_Phi_x2

# (1-Phi*B^24)*(1-B)x_2t = stationær AR(1) proces :
stat_x2 <- polymult(diff(x_t2),coefs_saeson_Phi_x2,1)
acf2(stat_x2)




acf2(x_t3)

acf2(diff(x_t3,1))

acf2(diff(x_t3,24))

acf2(diff(diff(x_t3,1),24))

seas_x3 = sarima(x_t3,2,0,0,2,1,0,24, no.constant = TRUE)

seas_x3res = ts(seas_x3$fit$residuals)

acf2(seas_x3res)

adf.test(seas_x3res) # stationær

coefs_x3 <- seas_x3$fit$coef
print(coefs_x3)

coefs_saeson_Phi_x3 <- c(1,rep(0,22),-coefs_x3[3:4])
coefs_saeson_Phi_x3

# (1-Phi*B^24)*(1-Phi*B^48)*(1-B)x_3t = stationær AR(2) proces :
stat_x3 <- polymult(diff(x_t3),coefs_saeson_Phi_x3,1)
acf2(stat_x3)




acf2(x_t4)

acf2(diff(x_t4,1))

acf2(diff(x_t4,24))

acf2(diff(diff(x_t4,1),24))

seas_x4 = sarima(x_t4,5,1,0,2,1,0,24, no.constant = TRUE)

seas_x4res = ts(seas_x4$fit$residuals)

acf2(seas_x4res)

adf.test(seas_x4res) # stationær

coefs_x4 <- seas_x4$fit$coef
print(coefs_x4)

coefs_saeson_Phi_x4 <- c(1,rep(0,22),-coefs_x4[6:7])
coefs_saeson_Phi_x4

# (1-Phi*B^24)*(1-Phi*B^48)*(1-B)x_4t = stationær AR(5) proces :
stat_x4 <- polymult(diff(x_t4),coefs_saeson_Phi_x4,1)
acf2(stat_x4)

### Sæson yt

acf2(y_t1)

acf2(diff(y_t1,1))

acf2(diff(y_t1,24))

acf2(diff(diff(y_t1,1),24))

seas_y1 = sarima(y_t1,2,0,0,1,0,0,24, no.constant = TRUE)

seas_y1res= ts(seas_y1$fit$residuals)

acf2(seas_y1res)

adf.test(seas_y1res) # stationær



coefs_y1 <- seas_y1$fit$coef
print(coefs_y1)

coefs_saeson_Phi_y1 <- c(1,rep(0,23),-coefs_y1[3])
coefs_saeson_Phi_y1

# (1-Phi*B^24)*(1-B)y_1t = stationær AR(2) proces :
stat_y1 <- polymult(diff(y_t1),coefs_saeson_Phi_y1,1)
acf2(stat_y1)



acf2(y_t2) 

acf2(diff(y_t2,1))

acf2(diff(y_t2,24))

acf2(diff(diff(y_t2,1),24))

seas_y2 = sarima(y_t2,2,0,0,1,0,0,24, no.constant = TRUE)

seas_y2res = ts(seas_y2$fit$residuals)

acf2(seas_y2res)

adf.test(seas_y2res) # stationær

coefs_y2 <- seas_y2$fit$coef
print(coefs_y2)

coefs_saeson_Phi_y2 <- c(1,rep(0,23),-coefs_y2[3])
coefs_saeson_Phi_y2

# (1-Phi*B^24)*(1-B)y_2t = stationær AR(2) proces :
stat_y2 <- polymult(diff(y_t2),coefs_saeson_Phi_y2,1)
acf2(stat_y2)




acf2(y_t3)

acf2(diff(y_t3,1))

acf2(diff(y_t3,24))

acf2(diff(diff(x_t3,1),24))

seas_y3 = sarima(y_t3,1,1,0,1,1,0,24, no.constant = TRUE)

seas_y3res = ts(seas_y3$fit$residuals)

acf2(seas_y3res)

adf.test(seas_y3res) # stationær


coefs_y3 <- seas_y3$fit$coef
print(coefs_y3)

coefs_saeson_Phi_y3 <- c(1,rep(0,23),-coefs_y3[2])
coefs_saeson_Phi_y3

# (1-Phi*B^24)*(1-B)y_3t = stationær AR(2) proces :
stat_y3 <- polymult(diff(y_t3),coefs_saeson_Phi_y3,1)
acf2(stat_y3)



acf2(y_t4)

acf2(diff(y_t4,1))

acf2(diff(y_t4,24))

acf2(diff(diff(y_t4,1),24))

seas_y4 = sarima(y_t4,4,1,0,2,1,0,24, no.constant = TRUE)

seas_y4res = ts(seas_y4$fit$residuals)

acf2(seas_y4res)

adf.test(seas_y4res) # stationær


coefs_y4 <- seas_y4$fit$coef
print(coefs_y4)

coefs_saeson_Phi_y4 <- c(1,rep(0,22),-coefs_y4[5:6])
coefs_saeson_Phi_y4

# (1-Phi*B^24)*(1-B)y_4t = stationær AR(4) proces :
stat_y4 <- polymult(diff(y_t4),coefs_saeson_Phi_y4,1)
acf2(stat_y4)


### Sæson zt
acf2(z_t1)

acf2(diff(z_t1,1))

acf2(diff(z_t1,24))

acf2(diff(diff(z_t1,1),24))

seas_z1 = sarima(z_t1,4,1,0,1,0,0,24, no.constant = TRUE)

seas_z1res= ts(seas_z1$fit$residuals)

acf2(seas_z1res)

adf.test(seas_z1res) # stationær


coefs_z1 <- seas_z1$fit$coef
print(coefs_z1)

coefs_saeson_Phi_z1 <- c(1,rep(0,23),-coefs_z1[5])
coefs_saeson_Phi_z1

# (1-Phi*B^24)*(1-B)z_1t = stationær AR(4) proces :
stat_z1 <- polymult(diff(z_t1),coefs_saeson_Phi_z1,1)
acf2(stat_z1)




acf2(z_t2) 

acf2(diff(z_t2,1))

acf2(diff(z_t2,24))

acf2(diff(diff(z_t2,1),24))

seas_z2 = sarima(z_t2,4,1,0,1,0,0,24, no.constant = TRUE)

seas_z2res = ts(seas_z2$fit$residuals)

acf2(seas_z2res)

adf.test(seas_z2res) # stationær

coefs_z2 <- seas_z2$fit$coef
print(coefs_z2)

coefs_saeson_Phi_z2 <- c(1,rep(0,23),-coefs_z2[5])
coefs_saeson_Phi_z2

# (1-Phi*B^24)*(1-B)z_2t = stationær AR(4) proces :
stat_z2 <- polymult(diff(z_t2),coefs_saeson_Phi_z2,1)
acf2(stat_z2)



acf2(z_t3)

acf2(diff(z_t3,1))

acf2(diff(z_t3,24))

acf2(diff(diff(z_t3,1),24))

seas_z3 = sarima(z_t3,3,1,0,1,0,0,24, no.constant = TRUE)

seas_z3res = ts(seas_z3$fit$residuals)

acf2(seas_z3res)

adf.test(seas_z3res) # stationær


coefs_z3 <- seas_z3$fit$coef
print(coefs_z3)

coefs_saeson_Phi_z3 <- c(1,rep(0,23),-coefs_z3[4])
coefs_saeson_Phi_z3

# (1-Phi*B^24)*(1-B)z_3t = stationær AR(3) proces :
stat_z3 <- polymult(diff(z_t3),coefs_saeson_Phi_z3,1)
acf2(stat_z3)


acf2(z_t4)

acf2(diff(z_t4,1))

acf2(diff(z_t4,24))

acf2(diff(diff(z_t4,1),24))

seas_z4 = sarima(z_t4,4,1,0,1,0,0,24, no.constant = TRUE)

seas_z4res = ts(seas_z4$fit$residuals)

acf2(seas_z4res)

adf.test(seas_z4res) # stationær

coefs_z4 <- seas_z4$fit$coef
print(coefs_z4)

coefs_saeson_Phi_z4 <- c(1,rep(0,23),-coefs_z4[5])
coefs_saeson_Phi_z4

# (1-Phi*B^24)*(1-B)z_4t = stationær AR(4) proces :
stat_z4 <- polymult(diff(z_t4),coefs_saeson_Phi_z4,1)
acf2(stat_z4)


seas_Xt1 = data.frame(stat_y1, stat_x1, stat_z1)[25:2181,]

seas_Xt2 = data.frame(stat_y2, stat_x2, stat_z2)[25:2183,]

seas_Xt3 = data.frame(stat_y3, stat_x3, stat_z3)[25:2207,]

seas_Xt4 = data.frame(stat_y4, stat_x4, stat_z4)[25:2208,]



seas_fit1 = VAR(seas_Xt1)

seas_fit2 = VAR(seas_Xt2)

seas_fit3 = VAR(seas_Xt3)

seas_fit4 = VAR(seas_Xt4)



summary(seas_fit1)

summary(seas_fit2)

summary(seas_fit3)

summary(seas_fit4)






