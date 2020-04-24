### SERIEKORRELATION
serial.test(fit1, type = "BG")#p - værdi mindre end 0.05. Reject H_0

serial.test(fit2, type = "BG")#p - værdi mindre end 0.05. Reject H_0

serial.test(fit3, type = "BG")#p - værdi mindre end 0.05. Reject H_0

serial.test(fit4, type = "BG")#p - værdi mindre end 0.05. Reject H_0


### HOMOSKEDASDICITET
arch.test(fit1)#p - værdi mindre end 0.05. Reject H_0

arch.test(fit2)#p - værdi mindre end 0.05. Reject H_0

arch.test(fit3)#p - værdi mindre end 0.05. Reject H_0

arch.test(fit4)#p - værdi mindre end 0.05. Reject H_0


###Normalitet

normality.test(fit1)#p - værdi mindre end 0.05. Reject H_0

normality.test(fit2)#p - værdi mindre end 0.05. Reject H_0

normality.test(fit3)#p - værdi mindre end 0.05. Reject H_0

normality.test(fit4)#p - værdi mindre end 0.05. Reject H_0



###Stationaritet

adf.test(y_t1)#p - værdi mindre end 0.05. Reject H_0

adf.test(y_t2)#p - værdi mindre end 0.05. Reject H_0

adf.test(y_t3)#p - værdi mindre end 0.05. Reject H_0

adf.test(y_t4)#p - værdi mindre end 0.05. Reject H_0



adf.test(x_t1)#p - værdi mindre end 0.05. Reject H_0

adf.test(x_t2)#p - værdi mindre end 0.05. Reject H_0

adf.test(x_t3)#p - værdi mindre end 0.05. Reject H_0

adf.test(x_t4)#p - værdi mindre end 0.05. Reject H_0



adf.test(z_t1)#p - værdi mindre end 0.05. Reject H_0

adf.test(z_t2)#p - værdi mindre end 0.05. Reject H_0

adf.test(z_t3)#p - værdi mindre end 0.05. Reject H_0

adf.test(z_t4)#p - værdi mindre end 0.05. Reject H_0





