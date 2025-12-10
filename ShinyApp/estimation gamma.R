
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
a <- function(b){
  return (nbr_pts*log(b)+sum(log(delta_x))+L^2/(b*x[L]))
}
fct = sapply(t[1:L],a)

