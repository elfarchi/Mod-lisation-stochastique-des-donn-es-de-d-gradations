mu = 15
sigma = 7
L = 1000
T = 10
N=1000

t = seq(0,T,length.out = L)
B = rep(0,L)
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


X= X_B[1,]
B=X_B[2,]


delta_res =X[2:L]-X[1:L-1]
plot(t, X,type= 'l')
mu_estimé = X[L]/(L*0.01)
sigma_est = sqrt(sum((delta_res-mu*0.01)^2)/(L*0.01))
B_nouvelle = simulate(mu,sigma)[2,]
X_est = mu_estimé*t+sigma_est*B_nouvelle
lines(t,X_est,type='l',col = 'darkgreen')