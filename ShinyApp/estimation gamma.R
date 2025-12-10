
nbr_pts = 1000
L = nbr_pts
forme = 16
taux = 8
T = 10
N=1000
t = seq(0,T,length.out = nbr_pts)
U <- runif(L)
simulate = function() {
  x = numeric(nbr_pts)
  x[1] = 0
  delta_x <- qgamma(U, shape=forme*T/nbr_pts, rate=taux)
  for (i in 2:nbr_pts){
    x[i] = delta_x[i] + x[i - 1]
  }
  return(x)
}
x = simulate()
delta_x = x[2:L]-x[1:L-1]
plot(x,type='l')
forme_est = x[L]^2/(0.01*(L*sum(delta_x^2)-x[L]^2))
taux_est = (x[L]*L)/(L*sum(delta_x^2)-x[L]^2)
delta_nouveau = qgamma(runif(L),shape = forme_est*T/nbr_pts, rate = taux_est)
y = numeric(nbr_pts)
y[1] = 0
for (j in 2:nbr_pts){
  y[j]= delta_nouveau[j]+y[j-1]
}
lines(y,type='l',col = "orange2")
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

