
nbr_pts = 1000
L = nbr_pts
forme = 16
n = 1000
taux = 8
T = 10
N=1000
t = seq(0,T,length.out = nbr_pts)
U <- runif(L)
simulate = function() {
  U <- runif(L)
  x = numeric(nbr_pts)
  x[1] = 0
  delta_x <- qgamma(U, shape=forme*T/nbr_pts, rate=taux)
  for (i in 2:nbr_pts){
    x[i] = delta_x[i] + x[i - 1]
  }
  res = matrix(0,nrow = 2, ncol = nbr_pts)
  res[1,] = x
  res[2,] = delta_x
  return(res)
}
mat_res = simulate()
x = mat_res[1,]
delta_x = mat_res[2,]
plot(t,x,type='l')
forme_est = x[L]^2/(0.01*(L*sum(delta_x^2)-x[L]^2))
taux_est = (x[L]*L)/(L*sum(delta_x^2)-x[L]^2)
delta_nouveau = qgamma(U,shape = forme_est*T/nbr_pts, rate = taux_est)
y = numeric(nbr_pts)
y[1] = 0
for (j in 2:nbr_pts){
  y[j]= delta_nouveau[j]+y[j-1]
}
lines(t,y,type='l',col = "orange2")

#Calcul de biais 
simulation = replicate(n,simulate())
forme_ech = numeric(n)
taux_ech = numeric(n)
for(k in 1:n){
  taux_ech[k] = (simulation[1,L,k]*L)/(L*sum(simulation[2,,k]^2)-simulation[1,L,k]^2)
  forme_ech[k] = taux_ech[k]*simulation[1,L,k]/T
}
biais_forme = mean(forme_ech)-forme
biais_taux = mean(taux_ech)-taux
LOG <- log(delta_x)

a <- function(b){
  s <- 0
  
  for(i in 1:L){   # ✅ correct loop
    if (!is.na(LOG[i]) && is.finite(LOG[i])) {   # ✅ correct test
      s <- s + LOG[i]
    }
  }
  
  return(nbr_pts * log(b) + s - nbr_pts * digamma(b * x[L] / L))
}

ind = seq(1,20,length.out = 200)
fct = sapply(ind,a)
res <- uniroot(a, interval = c(0.001, 10))
r = res$root
plot(ind,fct,type = 'l')
abline(h = 0, col = "red")          # ligne y = 0
abline(v = res$root, col = "blue") # position du zéro
points(res$root, 0, pch = 19)
idx <- which(fct[-1] * fct[-length(fct)] < 0)
cat("biais de la forme a :",biais_forme,"\n")
cat("biais du taux b :",biais_taux)
