df <- read.csv2("/home/elfarchi/equipe_1/Semicond.csv")
mu = mu_vect[1]
sigma = sigma_vect[1]
L = 1500
it = L/4
T = 20
N=1300
h = 30
x <- log(df[[1]])
t = seq(0,50,length.out = L)
simulate = function(mu,sigma) {
  B = rep(0,L)
  Z_k = rnorm(N)
  for (i in 1:L){
    u = t[i]
    s <- 0
    for(j in 1:N){
      e_ju = sqrt(8*T)/((2*j+1)*pi)*sin(((2*j+1)*pi*u)/(2*T))
      s <- s+ Z_k[j]*e_ju
    }
    B[i] = s
  }
  X = mu*t + sigma*B
  res = matrix(0,nrow = 2,ncol = L)
  res[1,] = X
  res[2,] = B
  return(res)
}
X_B = simulate(mu,sigma)

n = 100
X= X_B[1,]
B = X_B[2,]
#ARD proportionnel (ARD1)
X_maintenance = rep(0,L)
rho = 0.5
X_maintenance[(L/4*0):(L/4*(0+1))] = X[(L/4*0): (L/4*(0+1))]
t_maintenance = c(t[L/4],t[L/2],t[3*L/4],t[L])
plot(t, X,type= 'l')
for(i in 1:3){
  X_maintenance[(L/4*i)] = (1-rho)*X[(L/4*i)]
  delta = X[(L/4*i)]-X_maintenance[(L/4*i)]
  X_maintenance[((L/4*i)+1):((L/4*(i+1))-1)] = X[((L/4*i)+1):((L/4*(i+1))-1)]-delta
  abline(v = t_maintenance[i],col = 'red')
}
X_maintenance[L] =X[L]-delta
lines(t,X_maintenance, type ='l', col="blue")

#Ard fixe
X_maintenancefixe= rep(0,L)
petit_delta = 1
plot(t, X,type= 'l')
X_maintenancefixe[(L/4*0):(L/4*(0+1))] = X[(L/4*0): (L/4*(0+1))]
for(i in 1:3){
  X_maintenancefixe[((L/4*i)):((L/4*(i+1))-1)] = X[((L/4*i)):((L/4*(i+1))-1)]-petit_delta*i
  abline(v = t_maintenance[i],col = 'red')
}
X_maintenancefixe[L] =X[L]-petit_delta*3
lines(t,X_maintenancefixe, type ='l', col="green")

#Changement de drift
plot(t,X,type ='l')
X_drift = rep(0,L)
alpha = 0.8
X_drift[(L/4*0):(L/4*(0+1))] = X[(L/4*0): (L/4*(0+1))]
X = X - mu*t
for(i in 1:3){
  alpha = alpha/1.1
  X = X+mu*alpha*t
  X_drift[((L/4*i)):((L/4*(i+1))-1)] = X[((L/4*i)):((L/4*(i+1))-1)]
  X= X-mu*alpha*t
  abline(v = t_maintenance[i],col = 'red')
}
X_drift[L]=(X+mu*alpha*t)[L]
lines(t,X_drift,type='l',col = "cyan")



