
df <- read.csv2("/home/zeroualima/Desktop/equipe_1/Semicond.csv")
t = df[[1]]
x <- log(df[[1]])
n = length(x)
m = length(df[-1])
y_mat = matrix(0,nrow=length(df[-1]), ncol = length(df[-1][,1]))
for(i in 1:length(df[-1])){
  y_mat[i,] = log(df[-1][,i])
}  
y = y_mat[,2:n]-y_mat[,1:(n-1)]
z = matrix(0,nrow=length(df[-1]), ncol = length(df[-1][,1]))
for (j in 1:length(df[-1])){
  z[j,]= c(y_mat[j,1],y[j,])
}  
delta_t = c(x[1],x[2:n]-x[1:(n-1)])
d_tmat = matrix(delta_t, nrow = m, ncol = length(delta_t), byrow = TRUE)
mu = sum(z)/(x[n]*m)
sigma = sqrt(1/(n*m)*sum(((z-mu*d_tmat)^2)/d_tmat))
sim <- rnorm(n, mean = mu*delta_t, sd = sigma*sqrt(delta_t))
y_sim <- cumsum(sim) 
matplot(
    x, log(df[-1]),
    type = "b",
    pch = 16,
    lty = 1,
    xlab = "X",
    ylab = "Valeurs",
    main = "Une courbe par colonne",
   )
lines(x,y_sim,col ='gold',type = 'b')
  
  