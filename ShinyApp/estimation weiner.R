df <- read.csv2("/home/elfarchi/equipe_1/Semicond.csv")
mu = mu_vect[1]
sigma = sigma_vect[1]
L = 1000
T = 20
N=500
h = 2.5
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
  return(X)
}
X = simulate(mu,sigma)

n = 100

delta_res =X[2:L]-X[1:L-1]
plot(t, X,type= 'l')
mu_estimé = X[L]/(L*0.01)
sigma_est = sqrt(sum((delta_res-mu*0.01)^2)/(L*0.01))
X_rep = replicate(n,simulate(mu,sigma))
plot(t,X_est, type = 'l', col = "darkgreen")
for(i in 1:n){
  lines(t,X_rep[,i],type='l')
}
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

hist(liste_t, prob = TRUE, breaks = 20,
     main = "Histogramme de liste_t + densité IG théorique",
     xlab = "Temps de première traversée")

xx <- seq(min(liste_t), max(liste_t), length.out = 200)

lines(xx, dinvgauss(xx, mean = h/mu, shape = h^2/(sigma^2)),
      col = "red", lwd = 2)
# densité du temps de première traversée (Inverse Gaussienne)
dT_wiener <- function(t, h, mu, sigma) {
  # t peut être un vecteur
  out <- h / sqrt(2*pi*sigma^2 * t^3) * exp(-(h - mu*t)^2 / (2*sigma^2*t))
  out[t <= 0] <- 0
  out
}
dT = dT_wiener(t,h,mu,sigma)

  