
df <- read.csv2("/home/elfarchi/equipe_1/Semicond.csv")
x <- c(0,df[[1]])
n = length(x)
p = n-1
for(i in 1:length(df[-1])){
  y_col <- df[-1][,i]
  matplot(
    x[2:n], y_col,
    type = "b",
    pch = 16,
    lty = 1,
    xlab = "X",
    ylab = "Valeurs",
    main = "Une courbe par colonne")
  y = c(y_col[1],y_col[2:p]-y_col[1:(p-1)])
  delta_t = x[2:n]-x[1:(n-1)]
  mu =  y_col[p]/x[n]
  sigma = sqrt(1/p*sum(((y-mu*delta_t)^2)/delta_t))
  sim <- rnorm(p, mean = mu*delta_t, sd = sigma*sqrt(delta_t))
  y_sim =c(0,cumsum(sim))
  lines(x,y_sim,col ='blue',type = 'b')
}