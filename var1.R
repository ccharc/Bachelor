XXX1 = data.frame(stat_x1,stat_z1,stat_y1)[8:365,]


var_xx1 = VAR(XXX1, lag.max = 8)

var1 = restrict(var_xx1, method = "ser")


irf = irf(var1, ortho = TRUE)

plot(irf)
