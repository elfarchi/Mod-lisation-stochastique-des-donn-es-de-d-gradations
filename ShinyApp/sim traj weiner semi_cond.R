df <- read.csv2("/home/elfarchi/equipe_1/Semicond.csv")
t = df[[1]]
x <- log(df[[1]])
n = length(x)
m = length(df[-1])
mu_vect = numeric(m)
sigma_vect = numeric(m)
t = df[[1]]
log_t = log(t)
Y = log(df[-1])
n = length(t)
delta_t = diff(log_t)
delta_X = apply(Y,2,diff)
s <- sapply(1:ncol(Y),function(i){
  sum(delta_X[,i])
})
mu <- mean(s)/sum(delta_t)
sigma <- sqrt(var(s)/sum(delta_t))
Y = log(df[-1])
for(i in 1:length(df[-1])){
  y_col <- log(df[-1][,i])
  y = diff(y_col) #1
  delta_t = diff(x) #2
  mu =  (y_col[n]-y_col[1])/(x[n]-x[1]) #3
  mu_vect[i] = mu
  sigma = sqrt(1/n*sum(((y-mu*delta_t)^2)/delta_t))
  sigma_vect[i] = sigma
}
sim_mat <- sapply(1:length(mu_vect), function(i) {
  rnorm(length(t),mean = mu_vect[i]*delta_t, sd = sigma_vect[i]*sqrt(delta_t))
})
sim_maty= sapply(1:ncol(sim_mat), function(j){
  cumsum(sim_mat[,j])
})
matplot(x,Y,pch = 16, type = 'b')
  matplot(
    x, sim_maty,
    type = "b",
    pch = 16,
    lty = 1,
    xlab = "X",
    ylab = "Valeurs",
    main = "Une courbe par colonne",)
  simulateWiener = function(N, mu, sigma, x,t) {
    Z_k = rnorm(N) # Coefficients aléatoires
    n = length(x)# Stockage du bruit brownien approché
    B = rep(0,n)
    for (i in 1:n){
      u = t[i]
      s <- 0
      # Décomposition de Karhunen-Loève tronquée
      for(j in 1:N){
        e_ju = sqrt(8*t[n])/((2*j+1)*pi)*sin(((2*j+1)*pi*u)/(2*t[n]))
        s <- s+ Z_k[j]*e_ju
      }
      B[i] = s
    }
    # Processus avec dérive : X(t) = μt + σB(t)
    X = mu*x + sigma*B
    return(log(X))
  }
  sim_mat_2 <- sapply(1:length(mu_vect), function(i) {
    simulateWiener(1000,mu_vect[i],sigma_vect[i],x,t)
  })
  sim_maty_2= sapply(1:ncol(sim_mat), function(j){
    cumsum(sim_mat_2[,j])
  })
  