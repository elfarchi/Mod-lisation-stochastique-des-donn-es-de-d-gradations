
df <- read.csv2("/home/elfarchi/equipe_1/Laser.csv")
x <- df[[1]]
a_vect = numeric(length(df[-1]))
b_vect =  numeric(length(df[-1]))
y_sim = matrix(0,nrow = length(df[-1]),ncol = length(df[-1][,1]))
for(i in 1:length(df[-1])){
  y_col <- df[-1][,i]
  y =diff(y_col)
  m = mean(y)
  v = var(y)
  b = m/v
  b_vect[i]= b
  a = m^2/(v*diff(x)[1])
  a_vect[i] = a
  sim = rgamma((length(y)+1),shape = a*diff(x)[1], rate = b)
  y_sim_2 = cumsum(sim)
  y_sim[i,] = y_sim_2
  plot(x,y_col,type ='b')
  lines(x,y_sim[i,],type='b',col ="green") 
  #lines(x,y_sim_1,col ='darkgreen',type = 'b')
  #plot(c(x,x+4000),y_sim_2,col='hotpink',type = "b")
}
matplot(x, t(y_sim), type = "b", pch = 16, lty = 1, xlab = "X", ylab = "Valeurs", 
        main = "Une courbe par colonne")