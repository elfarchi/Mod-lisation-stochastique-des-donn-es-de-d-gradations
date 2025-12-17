n = 150
mu = 2
sigma = 5
L = 1200
T = 90
N=600
h = 70
alpha = 1.2
beta <- 1.5
rho = 0.7
nbr_maint = 4
petit_delta = 3
simulateWiener = function(N, L, mu, sigma, T) {
  Z_k = rnorm(N)          # Coefficients aléatoires
  t = seq(0,T,length.out = L)
  B = rep(0,L)            # Stockage du bruit brownien approché
  for (i in 1:L){
    u = t[i]
    s <- 0
    # Décomposition de Karhunen-Loève tronquée
    for(j in 1:N){
      e_ju = sqrt(8*T)/((2*j+1)*pi)*sin(((2*j+1)*pi*u)/(2*T))
      s <- s+ Z_k[j]*e_ju
    }
    B[i] = s
  }
  
  # Processus avec dérive : X(t) = μt + σB(t)
  X = mu*t + sigma*B
  return(X)
}
X = simulateWiener(N,L,mu,sigma,T)

delta_res =X[2:L]-X[1:L-1]
mu_estimé = X[L]/(L*0.01)
sigma_est = sqrt(sum((delta_res-mu*0.01)^2)/(L*0.01))
X_rep = replicate(n,simulateWiener(N,L,mu,sigma,T))
t = seq(0,T,length.out = L)
#plot(t,X_est, type = 'l', col = "darkgreen")
liste_t = numeric(n)
for(k in 1:n){
  liste_t[k]=t[which(X_rep[,k]>=h)[1]]
}
library(statmod)

liste_t <- liste_t[!is.na(liste_t)]

dT_wiener <- function(t, h, mu, sigma) {
  out <- h / sqrt(2*pi*sigma^2 * t^3) * exp(-(h - mu*t)^2 / (2*sigma^2*t))
  out
}
dT = dT_wiener(t,h,mu_estimé,sigma_est)
T = 80
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
  #abline(v = t_maintenance[2:(length(t_maintenance) - 1)],
  #       col = "red", lwd = 1)
  X_maintenance[L] =X[L]-delta
  #lines(t,X_maintenance, type ='l', col="blue")
  return(X_maintenance)
}
rho = 0.7
nbr_maint = 4
t = seq(0,T,length.out = L)
sim_ARD_1 <- function(X_rep){
  X_maint_rep <- matrix(0, nrow = nrow(X_rep),ncol= ncol(X_rep))
  for(i in 1:ncol(X_rep)){
    X_maint_rep[,i] = ARD_1(X_rep[,i],X_maint_rep[,i],rho,nbr_maint,T,L)
  }
  return(X_maint_rep)
}
X_maint_rep <- sim_ARD_1(X_rep)
for(i in 1:10){
  plot(t,X_rep[,i],type='l',col ="black")
  lines(t,X_maint_rep[,i],type='l',col = "blue")
}
liste_t_maint = numeric(n)
for(k in 1:n){
  liste_t_maint[k]=t[which(X_maint_rep[,k]>=h)[1]]
}
liste_t_maint <- liste_t_maint[!is.na(liste_t_maint)]
hist(liste_t_maint, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t + densité IG théorique",
     xlab = "Temps de première traversée",col = "blue")
hist(liste_t, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t + densité IG théorique",
     xlab = "Temps de première traversée")
xx <- seq(min(liste_t), max(liste_t), length.out = 200)

lines(xx, dinvgauss(xx, mean = h/mu, shape = h^2/(sigma^2)),
      col = "red", lwd = 2)
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
  X_maintenancefixe[L] =X[L]-petit_delta*(length(idx_maint)-1)
  return(X_maintenancefixe)
}

sim_ARD_fixe<- function(X_rep){
  X_maint_rep <- matrix(0, nrow = nrow(X_rep),ncol= ncol(X_rep))
  for(i in 1:ncol(X_rep)){
    X_maint_rep[,i] = ARD_fixe(X_rep[,i],X_maint_rep[,i],petit_delta,nbr_maint,T,L)
  }
  return(X_maint_rep)
}
X_fixerep = sim_ARD_fixe(X_rep)
for(i in 1:10){
  plot(t,X_rep[,i],type='l',col ="black")
  lines(t,X_fixerep[,i],type='l',col = "green")
}
liste_t_fixe = numeric(n)
for(k in 1:n){
  liste_t_fixe[k]=t[which(X_fixerep[,k]>=h)[1]]
}
liste_t_fixe <- liste_t_fixe[!is.na(liste_t_maint)]
hist(liste_t_fixe, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t + densité IG théorique",
     xlab = "Temps de première traversée",col = "darkgreen")
hist(liste_t, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t + densité IG théorique",
     xlab = "Temps de première traversée")


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
  X_drift[L]=(X+mu*alpha*t)[L]
  return(X_drift)
}
sim_drift<- function(X_rep){
  X_maint_rep <- matrix(0, nrow = nrow(X_rep),ncol= ncol(X_rep))
  for(i in 1:ncol(X_rep)){
    X_maint_rep[,i] = drift_change(X_rep[,i],X_maint_rep[,i],mu,alpha,beta,nbr_maint,T,L)
  }
  return(X_maint_rep)
}
X_driftrep = sim_drift(X_rep)
for(i in 1:10){
  plot(t,X_rep[,i],type='l',col ="black")
  lines(t,X_driftrep[,i],type='l',col = "cyan")
}
liste_t_drift = numeric(n)
for(k in 1:n){
  liste_t_drift[k]=t[which(X_driftrep[,k]>=h)[1]]
}
liste_t_drift <- liste_t_drift[!is.na(liste_t_maint)]
hist(liste_t, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t",
     xlab = "Temps de première traversée")
hist(liste_t_maint, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t (ARD_1)",
     xlab = "Temps de première traversée",col = "blue")
hist(liste_t_fixe, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t (ARD_fixe)",
     xlab = "Temps de première traversée",col = "darkgreen")
hist(liste_t_drift, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t (drift)",
     xlab = "Temps de première traversée",col = "cyan")
hist(liste_t, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t + densité IG théorique",
     xlab = "Temps de première traversée")























