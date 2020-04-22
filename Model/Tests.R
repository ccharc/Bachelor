serial.test(model1, type = "BG")#Der er seriekorrelation i fejledene. (Econometri)

arch.test(model1)

normality.test(model1)

efp(model1)

causality(model1)

irf(model1, seed = 123)

res = resid(model1)
acf(res)


stationary.test(x_t)


#test

#aic


#dickey fuller 
adf.test(y_t) #stationær lille p-værdi forkaster h0
adf.test(x_t) #stationær lille p-værdi forkaster h0
adf.test(z_t) #stationær lille p-værdi forkaster h0


#Breusch-Godfrey lm 
dwtest(model1)

w = bgtest(y_t~x_t)
coeftest(w)
w

r = bgtest(y_t~ x_t)
coeftest(r)
r

