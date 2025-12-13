library(statmod)
# densité du temps de première traversée (Inverse Gaussienne)
dT_wiener <- function(t, h, mu, sigma) {
  # t peut être un vecteur
  out <- h / sqrt(2*pi*sigma^2 * t^3) * exp(-(h - mu*t)^2 / (2*sigma^2*t))
  out[t <= 0] <- 0
  out
}
h <- 10
mu <- 6.675397e-05
sigma <- 0.01347132

tt <- seq(1e-6, 500000, length.out = 20000)
plot(tt, dT_wiener(tt, h, mu, sigma), type="l",
     xlab="t", ylab="f_T(t)",
     main="Densité du temps de défaillance (Wiener) : Inverse Gaussienne")
lines(tt, dinvgauss(tt, mean=h/mu, shape=(h^2)/(sigma^2)),col= "red",type='l')
