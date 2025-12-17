df <- read.csv2("/home/elfarchi/equipe_1/Laser.csv")
t <- df[[1]]
T <- sum(diff(t))
Y <- df[-1]
nbr_pts <- nrow(Y)
traj_nbr <- ncol(df[-1])
deltaY   <- apply(Y, 2, diff)      # matrix of increments (N rows × M columns)
mean_x   <- mean(deltaY)
mean_log <- mean(log(deltaY))
score <- function(r) log(r) - digamma(r) - (log(mean_x) - mean_log)
function(b, s, x_n, nbr_pts,traj_nbr) {
  nbr_pts * traj_nbr*log(b) + s - nbr_pts *traj_nbr* digamma(b * sum(x_n) /(nbr_pts*traj_nbr))
}
b_est <- uniroot(score, c(0.1, 20))$root    # ≈7.01336
sum_x  <- sum(Y[nbr_pts, ] - Y[1, ])
a_est  <- b_est * (nbr_pts * traj_nbr) / sum_x  # ≈13.65884










ind <- seq(1, 200000, length.out = 50000)
x_n_vect <- numeric(traj_nbr)
s_vect <- sapply(1:traj_nbr, function(i) {
  traj_i <- Y[, i]
  delta_Xi <- diff(traj_i)
  Log_delta_Xi <- log(delta_Xi)
  Log_delta_Xi <- Log_delta_Xi[!is.infinite(Log_delta_Xi)]
  sum(Log_delta_Xi)
})
s <- sum(s_vect)
x_n_vect <- sapply(1:traj_nbr,function(i){
  Y[,i][nbr_pts]-Y[,i][1]
})

func <- function(b, s, x_n, nbr_pts,traj_nbr) {
  nbr_pts * traj_nbr*log(b) + s - nbr_pts *traj_nbr* digamma(b * sum(x_n) /(nbr_pts*traj_nbr))
}
fct <- sapply(ind,function(b){
  func(b, s, x_n_vect, nbr_pts,traj_nbr)
})
idx <- which(fct[-1]* fct[-length(fct)] < 0)
b_est = ind[idx[1]]
a_est= (b_est*sum(x_n_vect))/(traj_nbr*T)
res <- c(a_est,b_est)