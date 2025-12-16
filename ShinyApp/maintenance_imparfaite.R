df <- read.csv2("/home/elfarchi/equipe_1/Semicond.csv")
mu = mu_vect[1]
sigma = sigma_vect[1]
L = 1000
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
ARD_1 <- function(X,X_maintenance,rho,nbr_maint,T,L){
  nbr_maint = nbr_maint+1
  idx_maint <- floor(seq(1, L, length.out = nbr_maint + 2))
  t <- seq(0, T, length.out = L)
  t_maintenance = rep(0,nbr_maint+1)
  
  for( i in 1:(nbr_maint+1)){
    t_maintenance[i]= t[idx_maint[i]]
  }
  X_maintenance[1:(idx_maint[2]-1)] = X[1: (idx_maint[2]-1)]
  delta <- 0
  for(i in 2:(length(idx_maint)-1)){
    X_maintenance[idx_maint[i]] = (1-rho)*X[idx_maint[i]]
    delta = X[idx_maint[i]]-X_maintenance[idx_maint[i]]
    X_maintenance[(idx_maint[i]+1):(idx_maint[i+1]-1)] = X[(idx_maint[i]+1):(idx_maint[i+1]-1)]-delta
  }
  abline(v = t_maintenance[2:(length(t_maintenance) - 1)],
         col = "red", lwd = 1)
  X_maintenance[L] =X[L]-delta
  lines(t,X_maintenance, type ='l', col="blue")
}
ARD_fixe <- function(X,X_maintenancefixe,petit_delta,nbr_maint,T,L){
  nbr_maint = nbr_maint+1
  idx_maint <- floor(seq(1, L, length.out = nbr_maint + 2))
  t <- seq(0, T, length.out = L)
  t_maintenance = rep(0,nbr_maint+1)
  for( i in (1:nbr_maint+1)){
    t_maintenance[i]= t[idx_maint[i]]
  }
  X_maintenancefixe[1:(idx_maint[2]-1)] = X[1: (idx_maint[2]-1)]
  for(i in 2:(length(idx_maint)-1)){
    X_maintenancefixe[idx_maint[i]:(idx_maint[i+1]-1)] = X[(idx_maint[i]):(idx_maint[i+1]-1)]-petit_delta*i
  }
  abline(v = t_maintenance[2:(length(t_maintenance) - 1)],
         col = "red", lwd = 1)
  X_maintenancefixe[L] =X[L]-petit_delta*(length(idx_maint)-1)
  lines(t,X_maintenancefixe, type ='l', col="green")
}
drift_change <- function(X,X_drift,mu,alpha,beta,nbr_maint,T,L){
  nbr_maint = nbr_maint+1
  idx_maint <- floor(seq(1, L, length.out = nbr_maint + 1))
  t <- seq(0, T, length.out = L)
  t_maintenance = rep(0,nbr_maint+1)
  for( i in 1:(nbr_maint+1)){
    t_maintenance[i]= t[idx_maint[i]]
  }
  X_drift[1:(idx_maint[2]-1)] = X[1: (idx_maint[2]-1)]
  X = X - mu*t
  for(i in 2:(length(idx_maint)-1)){
    alpha = alpha/beta
    X = X+mu*alpha*t
    X_drift[idx_maint[i]:(idx_maint[i+1]-1)] = X[(idx_maint[i]):(idx_maint[i+1]-1)]
    X= X-mu*alpha*t
  }
  abline(v = t_maintenance[2:(length(t_maintenance) - 1)],
         col = "red", lwd = 1)
  X_drift[L]=(X+mu*alpha*t)[L]
  lines(t,X_drift,type='l',col = "cyan")
}


