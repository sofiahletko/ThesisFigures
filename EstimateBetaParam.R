library(dplyr)
library(MASS)
library(rstan)
# true parameters for the beta distribution
alpha <- 3
beta <- 5 # beta = c(beta)
#           where c is determined by the coupon
# time constraints
a <- 14 # min time to next renewal
b <- 90 # max time to next renewal

# helper functions
time_scaler <- function(t){
  # takes in unscaled time (0 < t < 1), returns t on scale of beta
  # where a < t < b
  if(length(t) == 1){
    if(t < 0 || t > 1){
      stop("Error: Input must be between 0 and 1.")
    }
    shifted_scaled_time <- ((b-a)*t) + a
    return(shifted_scaled_time)
  } else{
    if (any(t < 0) || any(t > 1)){
      stop("Error: All values must be between 0 and 1.")
    }
    shifted_scaled_time <- ((b-a)*t) + a
    return(shifted_scaled_time)
  }
}
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

# or input observed data!
simulated_data <- time_scaler(rbeta(n = 1000, shape1 = alpha, shape2 = beta))
mle <- fitdistr(time_unscaler(simulated_data), densfun = "beta", 
                start = list(shape1 = 1, shape2 = 1),   # numeric starting values
                lower = c(0.001, 0.001) # beta distribution has the limit of positive numbers
                )
# the estimates are mle$estimate
print(mle$estimate)








# bayesian 
stan_data <- list(
  N = length(simulated_data),
  y = time_unscaler(simulated_data)
)
compiled_model_beta_param <- stan_model("EstimatingBetaParam.stan")
fit_beta_param <- sampling(compiled_model_beta_param, data = stan_data, 
                           chains = 4, iter = 1000)

