### SARMA 

### Sæson xt

acf2(x_t1)

acf(SE2prod)

#acf(x_t2)

#acf(x_t3)

#acf(x_t4)

seas_x1 = sarima(x_t1,4,1,0,1,0,0,24, no.constant = TRUE)

res_x1 = ts(seas_x1$residuals)[1:2181]

acf(res_x1)


seas_x2 = sarima(x_t2,4,1,0,1,0,0,24,no.constant = TRUE)

res_x2 = ts(seas_x2$residuals)

acf(res_x2)


seas_x3 = sarima(x_t3,4,1,0,1,0,0,24,no.constant = TRUE)

res_x3 = ts(seas_x3$residuals)

acf(res_x3)


seas_x4 = sarima(x_t4,4,1,0,1,0,0,24,no.constant = TRUE)

res_x4 = ts(seas_x4$residuals)

acf(res_x4)


### Sæson yt


#acf(y_t1)

#acf(y_t2)

#acf(y_t3)

#acf(y_t4)


seas_y1 = sarima(y_t1,4,1,0,1,0,0,24,no.constant = TRUE)

res_y1 = ts(seas_y1$residuals)

acf(res_y1)

seas_y2 = sarima(y_t2,4,1,0,1,0,0,24,no.constant = TRUE)

res_y2 = ts(seas_y2$residuals)

acf(res_y2)

seas_y3 = sarima(y_t3,4,1,0,1,0,0,24,no.constant = TRUE)

res_y3 = ts(seas_y3$residuals)

acf(res_y3)

seas_y4 = sarima(y_t4,4,1,0,1,0,0,24,no.constant = TRUE)

res_y4 = ts(seas_y4$residuals)

acf(res_y4)


### Sæson zt


#acf(z_t1)

#acf(z_t2)

#acf(z_t3)

#acf(z_t4)


seas_z1 = sarima(z_t1,4,1,0,1,0,0,24,no.constant = TRUE)

res_z1 = ts(seas_z1$residuals)[1:2181]



seas_z2 = sarima(z_t2,4,1,0,1,0,0,24,no.constant = TRUE)

res_z2 = ts(seas_z2$residuals)



seas_z3 = sarima(z_t3,4,1,0,1,0,0,24,no.constant = TRUE)

res_z3 = ts(seas_z3$residuals)



seas_z4 = sarima(z_t4,4,1,0,1,0,0,24,no.constant = TRUE)

res_z4 = ts(seas_z4$residuals)



seas_Xt1 = data.frame(res_y1, res_x1, res_z1)

seas_Xt2 = data.frame(res_y2, res_x2, res_z2)

seas_Xt3 = data.frame(res_y3, res_x3, res_z3)

seas_Xt4 = data.frame(res_y4, res_x4, res_z4)




seas_fit1 = VAR(seas_Xt1, ic = "AIC", lag.max = 6)

seas_fit2 = VAR(seas_Xt2, ic = "AIC", lag.max = 6)

seas_fit3 = VAR(seas_Xt3, ic = "AIC", lag.max = 6)

seas_fit4 = VAR(seas_Xt4, ic = "AIC", lag.max = 6)



summary(seas_fit1)

summary(seas_fit2)

summary(seas_fit3)

summary(seas_fit4)

