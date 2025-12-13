
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
plot(x,type='l')
forme_est = x[L]^2/(0.01*(L*sum(delta_x^2)-x[L]^2))
taux_est = (x[L]*L)/(L*sum(delta_x^2)-x[L]^2)
delta_nouveau = qgamma(U,shape = forme_est*T/nbr_pts, rate = taux_est)
y = numeric(nbr_pts)
y[1] = 0
for (j in 2:nbr_pts){
  y[j]= delta_nouveau[j]+y[j-1]
}
lines(y,type='l',col = "orange2")