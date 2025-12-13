
df <- read.csv2("/home/zeroualima/Desktop/equipe_1/Laser.csv")
x <- df[[1]]
a_vect = numeric(length(df[-1]))
b_vect =  numeric(length(df[-1]))
for(i in 1:length(df[-1])){
  y_col <- df[-1][,i]
  y = c(y_col[1],y_col[2:length(y_col)]-y_col[1:length(y_col)-1])
  m = mean(y)
  v = var(y)
  b = m/v
  b_vect[i]= b
  a = m^2/(v*250)
  a_vect[i] = a
  sim = rgamma(length(y),shape = a*250, rate = b)
  y_sim =cumsum(sim)
  matplot(x, y_col, type = "b", pch = 16, lty = 1, xlab = "X", ylab = "Valeurs", main = "Une courbe par colonne", ylim = c(min(y_sim,y_col),max(y_sim,y_col)))
  lines(x,y_sim,col ='darkgreen',type = 'b')
}
# 2nd part of simulation
mul= 4
p = 16*mul
t = seq(0,4000*mul,length.out = p)
fu <- function(){
  sim_1 = rgamma(p,shape = a_vect[1]*250,rate = b_vect[1])
  y_sim_1 = cumsum(sim_1)
  return(y_sim_1)

}
simi = fu()
plot(t,simi,type= 'b')
sim_rep = replicate(100,fu())
for(i in 1:100){
  lines(t,sim_rep[,i],type='b')
}
liste_t = numeric(100)
for(k in 1:100){
  liste_t[k]=t[which(sim_rep[,k]>=10)[1]]
}
library(statmod)
u <- rinvgauss(p, mean = 1, shape = 3)

hist(u, prob = TRUE, breaks = 30, main = "Loi inverse gaussienne")
xx <- seq(min(x), max(x), length.out = 200)
lines(xx, dinvgauss(xx, mean = 1, shape = 3))