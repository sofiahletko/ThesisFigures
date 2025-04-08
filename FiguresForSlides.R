library(dplyr)
library(ggplot2)
alpha <- 3
beta <- 5 # beta = c(beta)
#           where c is determined by the coupon



# time constraints
a <- 14 # min time to next renewal
b <- 90 # max time to next renewal

time_unscaler <- function(t){
  # takes in a scaled time a < t < b and puts it to a t that can 
  # be used in a beta, where 0 < t < 1
  if(length(t) == 1){
    if(t < a || t > b){
      stop("Error: Input must be between a and b.")
    }
    unscaled_time <- (t-a)/(b-a)
    return(unscaled_time)
  }else{
    if (any(t < a) || any(t > b)){
      stop("Error: All values must be between a and b.")
    }
    unscaled_time <- ((t - a) / (b - a))
    return(unscaled_time)
  }
  
}

times_unscaled <- seq(from = 0, to = 1, length.out = 100)
times_scaled <- seq(from = 14, to = 90, length.out = 100)

ggplot(data = data.frame(t=times_unscaled, 
                         d = dbeta(times_unscaled, shape1 = alpha, shape2 = beta)),
       aes(x=t, y=d))+geom_line() + labs(x="Time", y="Density", title = "Beta Distribution Density")

ggplot(data = data.frame(t=times_scaled, 
                         d = dbeta(time_unscaler(times_scaled), shape1 = alpha, shape2 = beta)),
       aes(x=t, y=d))+geom_line() + labs(x="Time",y="Density",  title = "Shifted Scaled Beta Distribution Density")





x_vals <-seq(from=0, to = 10, length.out = 100)


df <- data.frame(
  x = x_vals,
  y_0.5 = dexp(x_vals, rate = 0.5),
  y_1 = dexp(x_vals, rate = 1),
  y_2 = dexp(x_vals, rate = 2),
  y_3 = dexp(x_vals, rate = 3)
)

ggplot(data = df, aes(x = x)) +
  geom_line(aes(y = y_0.5, color = "Rate = 0.5")) +
  geom_line(aes(y = y_1, color = "Rate = 1")) +
  geom_line(aes(y = y_2, color = "Rate = 2")) +
  geom_line(aes(y = y_3, color = "Rate = 3")) +
  labs(title = "Exponential Distributions with Different Rates",
       x = "x", y = "Density", color = "Rate") +
  theme_minimal()