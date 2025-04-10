set.seed(24)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Simulation parameters
num_customers <- 100
n_sim <- 100
time_horizon <- 365

# Parameters for the beta distribution
alpha <- 3
beta <- 5 

# Time constraints
a <- 14 # min time to next renewal
b <- 90 # max time to next renewal

# Monte Carlo Approximation for the Beta distribution
beta_pdf_scaled_mc <- function(x, alpha, beta, a, b) {
  # Scale x to [0, 1] and compute PDF
  x_scaled <- (x - a) / (b - a)  # Map [a, b] to [0, 1]
  return((1 / (b - a)) * (x_scaled^(alpha - 1)) * ((1 - x_scaled)^(beta - 1)) / beta(alpha, beta))
}

# Define the Log-Log Approximation for the MGF of the Beta distribution
mgf_N_i_log_log_approx <- function(t, num_samples = 10000) {
  # Sample points from the Beta distribution over [a, b]
  samples <- runif(num_samples, a, b)  # Uniformly sample over the interval [a, b]
  
  # Compute the integrand at each sampled point and average the results
  log_log_integrand_values <- sapply(samples, function(x) {
    # Compute log-log approximation (log(exp(t * x)) -> t * x)
    log_exp_term <- t * x  # Log-log approximation for the integral
    
    beta_pdf_val <- beta_pdf_scaled_mc(x, alpha, beta, a, b)
    return(log(log_exp_term + 1e-10) * beta_pdf_val)  # Avoid log(0) by adding a small epsilon
  })
  
  # Compute the mean of the log-log integrand values
  log_log_mc_estimate <- mean(log_log_integrand_values)
  
  # Exponentiate the result for final MGF
  return(exp(log_log_mc_estimate))
}

# Log-Log Approximation for Poisson MGF
mgf_poisson_log_log_approx <- function(t, lambda) {
  # Log-log approximation for the Poisson MGF
  exp_term <- exp(t)
  
  # If exp(t) becomes large, subtract 1 to avoid overflow
  if (exp_term > 1e10) {
    exp_term_approx <- exp(t) - 1  # Safe computation for large exp(t)
  } else {
    exp_term_approx <- exp(t)  # Direct computation for smaller t
  }
  
  # Compute log-log approximation
  log_log_exp_term <- log(exp_term_approx + 1e-10)  # Small epsilon to avoid log(0)
  log_log_mgf <- lambda * log_log_exp_term
  
  # Return the exponentiated result
  return(exp(log_log_mgf))
}

# Set lambda for Poisson process
e_t <- a + (b - a) * (alpha / (alpha + beta))
lambda <- (1 / e_t) * num_customers

# Compute the MGF for a specific t value (e.g., t = a + 1)
t_value <- 15  # Choose a larger t value
mgf_result_pois_log_log <- mgf_poisson_log_log_approx(t_value, lambda)

# Compute the MGF for the Beta distribution using Log-Log Approximation
mgf_result_n_i_log_log <- mgf_N_i_log_log_approx(t_value)




t_vals <- c(1,2,3,4,5,6,7,8,9,10,11)
mgf_ssb <-c()
mgf_p <- c()
for(t in t_vals){
  mgf_ssb <- c(mgf_ssb, (mgf_N_i_log_log_approx(t)^num_customers))
  mgf_p <- c(mgf_p,mgf_poisson_log_log_approx(t, lambda))
}

data.frame(ssb=mgf_ssb, p = mgf_p)

# Print results
#print(paste("Log-Log Approximation MGF for Beta distribution at t =", t_value, "is", mgf_result_n_i_log_log))
print(paste("Log-Log Approximation MGF for Beta distribution at t =", t_value, "is", mgf_result_n_i_log_log^num_customers))
print(paste("Log-Log Approximation MGF for Poisson distribution with lambda =", lambda, "and t =", t_value, "is", mgf_result_pois_log_log))
