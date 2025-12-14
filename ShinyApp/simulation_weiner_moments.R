
df <- read.csv2("/home/elfarchi/equipe_1/Semicond.csv")
t = df[[1]]
x <- log(df[[1]])
n = length(x)
m = length(df[-1])
y_mat = matrix(0,nrow=length(df[-1]), ncol = length(df[-1][,1]))
for(i in 1:length(df[-1])){
  y_mat[i,] = log(df[-1][,i])
}  
y = y_mat[,2:n]-y_mat[,1:(n-1)]
delta_x = sapply(1:m, function(i){
  y_mat[i,n]-y_mat[i,1]
})
delta_t = diff(x)
d_tmat = matrix(delta_t, nrow = m, ncol = length(delta_t), byrow = TRUE)
mu = sum(delta_x)/((x[n]-x[1])*m)
sigma = sqrt(1/(n*m)*sum(((y-mu*d_tmat)^2)/d_tmat))
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


