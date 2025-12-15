b_true   <- 1.91
a_true  <-  1.25
sigma_B  <- 650        # diffusion coefficient
w        <- 71500    # failure threshold (mm)
t_max    <- 400    # last measurement time (months)
delta    <- 0.2        # sampling interval (months)
times    <- seq(0, t_max, by = delta)
n        <- length(times)
number_of_paths = 1000
{plot(times, X, type = "l", col = "red")}
abline(a= w, b=0)
time_to_failure = numeric(length = length(number_of_paths))
for (i in 1:number_of_paths){
  X_i<- cumsum(rnorm(n, mean = 0, sd = sqrt(delta)))   # scaled BM
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
     breaks = 40,            
     col = "skyblue",
     border = "white",
     main = "Histogram of Time to Failure",
     xlab = "Time to Failure",
     probability = TRUE)    



fM2 <- function(t, a, b, w, sigmaB) {
  num  <- w - a * t^b * (1 - b)
  den  <- sigmaB * sqrt(2 * pi * t^3)
  expo <- exp( - (w - a * t^b)^2 / (2 * sigmaB^2 * t) )
  return(num / den * expo)
}



tgrid <- seq(min(time_to_failure), max(time_to_failure), length.out = 500)



lines(tgrid, fM2(tgrid, a_true, b_true, w, sigma_B), col = "violet", lwd = 2)
