df <- read.csv2("/home/elfarchi/equipe_1/Laser.csv", check.names = FALSE)

t <- df[[1]]
Y <- df[-1]
dt <- diff(t); dt <- dt[1]
dX_all <- as.vector(apply(Y, 2, diff))

b_global <- mean(dX_all) / var(dX_all)
a_global <- mean(dX_all)^2 / (var(dX_all) * dt)
a_global; b_global
sim <- rgamma(length(t),shape = a_global*diff(t)[1],rate = b_global)
y_sim <- cumsum(sim)
#matplot(t,Y, type = "b",pch = 16, lty = 1)
plot(t,y_sim,type = 'l',col= "purple",lwd = 7)