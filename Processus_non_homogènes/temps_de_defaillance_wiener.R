b_true   <- 1.91
a_true  <-  1.25
sigma_B  <- 650        # coefficient de diffusion
w        <- 71500    # seuil  de franchissement
t_max    <- 400    # dernière date de mesure 
delta    <- 0.2        # lapse de temps entre les mesures
times    <- seq(0, t_max, by = delta)
n        <- length(times)
number_of_paths = 1000
plot(x = times,times,type="n")
abline(a= w, b=0)
time_to_failure = numeric(length = length(number_of_paths))
for (i in 1:number_of_paths){
  X_i<- cumsum(rnorm(n, mean = 0, sd = sqrt(delta)))   # simulation du processus de Wiener
  path <- a_true * times^b_true + sigma_B * X_i
  failure_time = times[path>= w][1]
  if (is.na(failure_time)){
    time_to_failure[i] = -1
    next
  }
  time_to_failure[i] =failure_time
  lines(times, path,  type = "l")
  
}


hist(time_to_failure,
     breaks = 35,
     col = "skyblue",
     border = "white",
     main = "Histogramme des temps de defaillance",
     xlab = "temps de defaillance",
     probability = TRUE)



fM2 <- function(t, a, b, w, sigmaB) {
  num  <- w - a * t^b * (1 - b)
  den  <- sigmaB * sqrt(2 * pi * t^3)
  expo <- exp( - (w - a * t^b)^2 / (2 * sigmaB^2 * t) )
  return(num / den * expo)
}



tgrid <- seq(min(time_to_failure), max(time_to_failure), length.out = 500)



lines(tgrid, fM2(tgrid, a_true, b_true, w, sigma_B), col = "violet", lwd = 2)
# Ajouter une légende
legend("topright", 
       legend = c("approximation de la densité théorique"),
       col = "violet",
       lwd = 2, 
       cex = 0.9)
