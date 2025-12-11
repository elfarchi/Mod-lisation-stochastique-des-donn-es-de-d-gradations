
df <- read.csv2("/home/elfarchi/equipe_1/Laser.csv")
x <- df[[1]]
for(i in 1:length(df[-1])){
  y_col <- df[-1][,i]
  matplot(
    x, y_col,
    type = "b",
    pch = 16,
    lty = 1,
    xlab = "X",
    ylab = "Valeurs",
    main = "Une courbe par colonne")
  y = y_col[2:length(y_col)]-y_col[1:length(y_col)-1]
  m = mean(y)
  v = var(y)
  b = m/v
  a = m^2/(v*250)
  sim = rgamma(length(y),shape = a*250, rate = b)
  y_sim =c(0,cumsum(sim))
  lines(x,y_sim,col ='blue',type = 'b')
}