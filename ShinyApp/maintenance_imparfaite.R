df <- read.csv2("/home/elfarchi/equipe_1/Semicond.csv")
mu = mu_vect[1]
sigma = sigma_vect[1]
L = 1500
it = L/4
T = 20
N=1000
h = 30
x <- log(df[[1]])
t = seq(0,x[35],length.out = L)
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
X_maintenance = rep(0,L)
rho = 0.5
X_maintenance[(L/4*0):(L/4*(0+1))] = X[(L/4*0): (L/4*(0+1))]

delta_res =X[2:L]-X[1:L-1]
plot(t, X,type= 'l')
s <- 0
for(i in 1:3){
  X_maintenance[(L/4*i)] = (1-rho)*X[(L/4*i)]
  delta = X[(L/4*i)]-X_maintenance[(L/4*i)]
  X_maintenance[((L/4*i)+1):((L/4*(i+1))-1)] = X[((L/4*i)+1):((L/4*(i+1))-1)]-delta
  abline(v = t_maintenance[i],col = 'red')
}
X_maintenance[L] =X[L]-delta
lines(t,X_maintenance, type ='l', col="blue")
