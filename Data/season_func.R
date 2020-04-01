rm(list=ls())

# Koefficientudregning af S(t)
scoef = function(fit){
  coef = as.numeric(fit$coefficients)
  cm = vcov(fit)
  
  # Koefficienter til årlig periode
  
  c1  = sqrt(coef[4]^2 + coef[5]^2)
  c2  = atan(coef[5]/coef[4]) * 365.25/(2*pi)
  
  # Koefficienter til halvårlig periode
  
  c3  = sqrt(coef[6]^2 + coef[7]^2)
  c4  = atan(coef[7]/coef[6]) * 365.25/(8*pi)
  
  # Koefficienter til kvartal periode
  
  c5  = sqrt(coef[8]^2 + coef[9]^2)
  c6  = atan(coef[9]/coef[8]) * 365.25/(24*pi) 
  
  # Koefficienter til månedlig periode
  
  c7 = sqrt(coef[10]^2 + coef[11]^2)
  c8 = atan(coef[11]/coef[10]) * 365.25/(24*pi)
  
  #Koefficienter til ugentlig periode
  
  c9 = sqrt(coef[12]^2 + coef[13]^2)
  c10 = atan(coef[13]/coef[12]) * 365.25/(24*pi)
  
  #Koefficienter til daglig periode
  
  c9 = sqrt(coef[14]^2 + coef[15]^2)
  c10 = atan(coef[15]/coef[14]) * 365.25/(24*pi)
  
  # Standardafvigelser til årlig periode
  
  sdc1 = sqrt( 1/(coef[4]^2 + coef[5]^2)*(coef[4]^2 * cm[4,4] + coef[5]^2*cm[5,5] + coef[4]*coef[5] * 2*cm[4,5]) )
  sdc2 = sqrt( (365.25/( 2*pi*(coef[4]^2 + coef[5]^2) ) )^2 * ( coef[5]^2 * cm[4,4] + coef[4]^2 * cm[5,5] - 2*coef[4]*coef[5]*cm[4,5])  )
  
  # Standardafvigelser til halvårlig periode
  
  sdc3 = sqrt( 1/(coef[6]^2 + coef[7]^2)*(coef[6]^2 * cm[6,6] + coef[7]^2*cm[7,7] + coef[6]*coef[7] * 2*cm[6,7]) )
  sdc4 = sqrt( (365.25/( 2*pi*(coef[6]^2 + coef[7]^2) ) )^2 * ( coef[7]^2 * cm[6,6] + coef[6]^2 * cm[7,7] - 2*coef[6]*coef[7]*cm[6,7])  )
  
  # Standardafvigelser til kvartal periode
  
  sdc5 = sqrt( 1/(coef[8]^2 + coef[9]^2)*(coef[8]^2 * cm[8,8] + coef[9]^2*cm[9,9] + coef[8]*coef[9] * 2*cm[8,9]) )
  sdc6 = sqrt( (365.25/( 2*pi*(coef[8]^2 + coef[9]^2) ) )^2 * ( coef[9]^2 * cm[8,8] + coef[8]^2 * cm[9,9] - 2*coef[8]*coef[9]*cm[8,9])  )
  
  # Standardafvigelse til månedlig periode
  
  sdc7 = sqrt( 1/(coef[10]^2 + coef[11]^2)*(coef[10]^2 * cm[10,10] + coef[11]^2*cm[11,11] + coef[10]*coef[11] * 2*cm[10,11]) )
  sdc8 = sqrt( (365.25/( 2*pi*(coef[10]^2 + coef[11]^2) ) )^2 * ( coef[11]^2 * cm[10,10] + coef[10]^2 * cm[11,11] - 2*coef[10]*coef[11]*cm[10,11])  )
  
  #Standardafvigelse til ugentlig periode
  
  sdc9 = sqrt( 1/(coef[12]^2 + coef[13]^2)*(coef[12]^2 * cm[12,12] + coef[13]^2*cm[13,13] + coef[12]*coef[13] * 2*cm[12,13]) )
  sdc10 = sqrt( (365.25/( 2*pi*(coef[12]^2 + coef[13]^2) ) )^2 * ( coef[13]^2 * cm[12,12] + coef[12]^2 * cm[13,13] - 2*coef[12]*coef[5]*cm[12,13])  )
  
  #Standardafvigelse til daglig periode 
  
  sdc11 = sqrt( 1/(coef[14]^2 + coef[15]^2)*(coef[14]^2 * cm[14,14] + coef[5]^2*cm[15,15] + coef[14]*coef[15] * 2*cm[14,15]) )
  sdc12 = sqrt( (365.25/( 2*pi*(coef[14]^2 + coef[15]^2) ) )^2 * ( coef[15]^2 * cm[14,14] + coef[14]^2 * cm[15,15] - 2*coef[14]*coef[15]*cm[14,15])  )
  
} 

# Dataframe med alt info
ls = list( coef = data.frame(b0 = coef[1] , b1 = coef[2] , b2 = coef[3] , c1 = c1 , c2 = c2 , c3 = c3 , c4 = c4 , 
                             c5 = c5 , c6 = c6 , c7 = c7 , c8 = c8 , c9 = c9 , c10 = c10 ), 
                             sdc  = data.frame(b0 = sqrt(cm[1,1]) , b1 = sqrt(cm[2,2]) , b2 = sqrt(cm[3,3]) , 
                                               c1 = sdc1 , c2 = sdc2 , c3 = sdc3 , c4 = sdc4 , c5 = sdc5 , c6 = sdc6 , 
                                               c7 =sdc7 , c8 = sdc8 , c9 = sdc9 , c10 = sdc10 , c11 = sdc11 , c12 = sdc12 ))
                             
           