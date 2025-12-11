df <- read.csv2("/home/elfarchi/equipe_1/Semicond.csv")
t = df[[1]]
x <- log(df[[1]])
n = length(x)
m = length(df[-1])
mu_vect = numeric(m)
sigma_vect = numeric(m)
Y = df[-1]
for(i in 1:length(df[-1])){
  y_col <- log(df[-1][,i])
  y = c(y_col[1],y_col[2:n]-y_col[1:(n-1)])
  delta_t = c(t[1],t[2:n]-t[1:(n-1)])
  mu =  y_col[n]/t[n]
  mu_vect[i] = mu
  sigma = sqrt(1/n*sum(((y-mu*delta_t)^2)/delta_t))
  sigma_vect[i] = sigma
}
mean_mu = mean(mu_vect)
mean_sigma = mean(sigma_vect)
sim <- rnorm(n, mean = mean_mu*delta_t, sd = mean_sigma*sqrt(delta_t))
y_sim =cumsum(sim)
  matplot(
    x, Y,
    type = "b",
    pch = 16,
    lty = 1,
    xlab = "X",
    ylab = "Valeurs",
    main = "Une courbe par colonne",
    ylim= c(min(y_col,y_sim),max(y_col,y_sim)))
  lines(x,y_sim,col ='hotpink',type = 'b')
