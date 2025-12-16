df <- read.csv2("/home/elfarchi/equipe_1/Laser.csv")
Tmax <- max(df[[1]])
X <- df[["U1"]]
delta_X <- c(X[1], diff(X))
s <- sum(log(delta_X))
n <- length(X)
x_n <- X[n]

f <- function(b) n*log(b) + s - n*digamma(b*x_n/n)

ind <- seq(0.5, 2e10, length.out = 50000)
vals <- sapply(ind, f)
plot(ind,vals,col="red",type='l')
range(vals)          # si tout est >0 (ou tout <0) => aucune racine
which(vals[-1]*vals[-length(vals)] < 0 ) # FALSE => aucun changement de signe
