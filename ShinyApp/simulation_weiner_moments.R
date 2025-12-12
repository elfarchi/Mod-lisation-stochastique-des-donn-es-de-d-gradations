
df <- read.csv2("/home/elfarchi/equipe_1/Semicond.csv")
t = df[[1]]
x <- log(df[[1]])
n = length(x)
m = length(df[-1])
y_mat = matrix(0,nrow=length(df[-1]), ncol = length(df[-1][,1]))
for(i in 1:length(df[-1])){
  y_mat[i,] = log(df[-1][,i])
}  
y = y_mat[,2:n]-y_mat[,1:(n-1)]
z = matrix(0,nrow=length(df[-1]), ncol = length(df[-1][,1]))
for (j in 1:length(df[-1])){
  z[j,]= c(y_mat[j,1],y[j,])
}  
delta_t = c(x[1],x[2:n]-x[1:(n-1)])
d_tmat = matrix(delta_t, nrow = m, ncol = length(delta_t), byrow = TRUE)
mu = sum(z)/(x[n]*m)
sigma = sqrt(1/(n*m)*sum(((z-mu*d_tmat)^2)/d_tmat))
sim <- rnorm(n, mean = mu*delta_t, sd = sigma*sqrt(delta_t))
y_sim <- cumsum(sim) 
matplot(
  x, log(df[-1]),
  type = "b",
  pch = 16,
  lty = 1,
  xlab = "X",
  ylab = "Valeurs",
  main = "Une courbe par colonne",
)
lines(x,y_sim,col ='gold',type = 'b')
# 2nd part of simulation
t = seq(1,10,length.out = 100)
mul= 6
h = 2.3
p = 35*mul
t = seq(0,40000*mul,length.out = p)
fu <- function(){
  sim_1 = rnorm(length(delta_t),mean = mu*delta_t, sd = sigma*sqrt(delta_t))
  y_sim_1 = cumsum(sim_1)
  return(y_sim_1)
  
}
simi = fu()
plot(delta,simi,type= 'b')
sim_rep = replicate(100,fu())
for(i in 1:100){
  lines(x,sim_rep[,i],type='b')
}
liste_t = numeric(100)
for(k in 1:100){
  liste_t[k]=x[which(sim_rep[,k]>=h)[1]]
}
library(statmod)

liste_t <- liste_t[!is.na(liste_t)]   # enlève NA

### --- Histogramme du temps de défaillance ---
hist(liste_t, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t + densité IG théorique",
     xlab = "Temps de première traversée")

xx <- seq(min(liste_t), max(liste_t), length.out = 200)

lines(xx, dinvgauss(xx, mean = h/mu, shape = h^2/(sigma^2)),
      col = "red", lwd = 2)

