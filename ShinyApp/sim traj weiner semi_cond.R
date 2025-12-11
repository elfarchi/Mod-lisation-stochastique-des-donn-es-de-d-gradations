
df <- read.csv2("/home/elfarchi/equipe_1/Semicond.csv")
x <- log(df[[1]])
n = length(x)
for(i in 1:length(df[-1])){
  y_col <- log(df[-1][,i])
  y = c(y_col[1],y_col[2:n]-y_col[1:(n-1)])
  delta_t = c(x[1],x[2:n]-x[1:(n-1)])
  mu =  y_col[n]/x[n]
  sigma = sqrt(1/n*sum(((y-mu*delta_t)^2)/delta_t))
  sim <- rnorm(n, mean = mu*delta_t, sd = sigma*sqrt(delta_t))
  y_sim =cumsum(sim)
  matplot(
    x, y_col,
    type = "b",
    pch = 16,
    lty = 1,
    xlab = "X",
    ylab = "Valeurs",
    main = "Une courbe par colonne",
    ylim= c(min(y_col,y_sim),max(y_col,y_sim)))
  lines(x,y_sim,col ='blue',type = 'b')
}