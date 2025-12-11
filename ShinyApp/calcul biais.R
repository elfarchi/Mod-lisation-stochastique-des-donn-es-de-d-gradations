nbr_pts = 1000
L = nbr_pts
n = 300
forme = 16
taux = 8
T = 10
N=1000
t = seq(0,T,length.out = nbr_pts)
simulate = function() {
  U <- runif(L)
  x = rep(0,L)
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
x_delta_x <- simulate()
res_req = replicate(n,simulate())

LOG_mass = log(res_req[2,,])
s<-rep(0,n)
for(p in 1:n){
  s[p] = s[p]+sum(LOG_mass[,p])
}
func <- function(b){
  res = matrix(0,nrow =1, ncol =n )
  for(k in 1:n){
    res[1,k] = nbr_pts * log(b) + s[k] - nbr_pts * digamma(b * res_req[1,L,k] / L)
  }
  return(res)
}
ind = seq(1,20,length.out = 200)
fct = sapply(ind,func)
b_ech = numeric(n)

for(j in 1:n){
  idx <- which(fct[j,][-1] * fct[j,][-length(fct[j,])] < 0)
  b_ech[j] = ind[idx[1]]
}
biais = mean(b_ech)-taux