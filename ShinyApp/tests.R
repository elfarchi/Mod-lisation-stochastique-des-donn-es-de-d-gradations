
df <- read.csv2("../Laser.csv")
x <- df[[1]]
for(i in 1:length(df[-1])){
  y_col <- df[-1][,i]
  y = c(y_col[1],y_col[2:length(y_col)]-y_col[1:length(y_col)-1])
  m = mean(y)
  v = var(y)
  b = m/v
  a = m^2/(v*250)
  sim = rgamma(length(y),shape = a*250, rate = b)
  y_sim =cumsum(sim)
  matplot(x, y_col, type = "b", pch = 16, lty = 1, xlab = "X", ylab = "Valeurs", main = "Une courbe par colonne", ylim = c(min(y_sim,y_col),max(y_sim,y_col)))
  lines(x,y_sim,col ='darkgreen',type = 'b')
}