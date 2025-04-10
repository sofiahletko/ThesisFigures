set.seed(24)
library(dplyr)
library(ggplot2)
library(tidyverse)
fig_filepath <- "~/Documents/Duke-Classes/IndependentStudy/github-repo/RFigures"


# Simulation parameters
num_customers <- 1000
n_sim <- 1000
#time_horizon <- 365
time_horizon <- 365*10

# parameters for the beta distribution
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

prob_cut_at_t <- function(t){
  # shifted scaled beta at time t
  numerator_p1 <-((t-a)/(b-a))^(alpha-1)
  numerator_p2 <- ((b-t)/(b-a))^(beta-1)
  numerator <- numerator_p1*numerator_p2
  denominator <- (b-a) * beta(alpha, beta)
  return(numerator/denominator)
}

generate_interarrival_time <- function(){
  unscaled <- rbeta(n = 1, shape1 = alpha, shape2 = beta)
  scaled <- (unscaled * (b-a)) + a
  return(scaled)
}

prob_cut_at_t_new_coupon <- function(t, c){
  # shifted scaled beta at time t
  numerator_p1 <-((t-a)/(b-a))^(alpha-1)
  numerator_p2 <- ((b-t)/(b-a))^((beta*c)-1)
  numerator <- numerator_p1*numerator_p2
  denominator <- (b-a) * beta(alpha, (beta*c))
  return(numerator/denominator)
}

# Function to simulate renewal process until reaching time_horizon
simulate_renewal_process_fixed_time <- function(time_horizon) {
  time <- 0
  event_times <- c()
  
  while (time < time_horizon) {
    time <- time + generate_interarrival_time()
    if (time < time_horizon) {
      event_times <- c(event_times, time)
    }
  }
  
  return(event_times)
}

# Superposition function for multiple sources
superpose_renewal_processes_fixed_time <- function(num_sources, time_horizon) {
  #all_events <- unlist(lapply(1:num_sources, time_horizon, function(x) {
  all_events <- unlist(lapply(1:num_sources, function(x) {
    simulate_renewal_process_fixed_time(time_horizon)
  }))
  return(sort(all_events))  # Sort to get the pooled event sequence
}

simulate_poisson_process <- function(lambda, time_horizon) {
  time <- 0
  event_times <- c()
  
  while (time < time_horizon) {
    time <- time + rexp(1, rate = lambda)  # Exponential interarrival times
    if (time < time_horizon) {
      event_times <- c(event_times, time)
    }
  }
  
  return(event_times)
}

# Function to simulate visits for one customer using generate_interarrival_time
simulate_customer_visits <- function(time_horizon) {
  event_times <- c()
  time <- 0
  while (time < time_horizon) {
    time <- time + generate_interarrival_time()  # Use your custom interarrival function
    if (time < time_horizon) {
      event_times <- c(event_times, time)
    }
  }
  return(event_times)
}

# Function to calculate total revenue for a simulation run
simulate_total_revenue <- function() {
  total_revenue <- 0
  total_visits <- 0
  
  # For each customer, simulate their visits and calculate the revenue
  for (cust in 1:num_sources) {
    customer_visits <- simulate_customer_visits(time_horizon)
    
    # Calculate revenue: 15 if before day 60, 20 after day 60
    revenue <- sum(ifelse(customer_visits <= coupon_time, coupon_price, price))
    
    total_revenue <- total_revenue + revenue
    total_visits <- total_visits + length(customer_visits)
  }
  
  # Calculate revenue rate per visit
  #revenue_rate <- total_revenue / total_visits
  revenue_rate <- total_revenue / time_horizon
  
  return(revenue_rate)
}


# Function to simulate total revenue from Poisson process
simulate_poisson_revenue <- function(lambda, time_horizon) {
  # Sample total number of events (visits) in the time horizon
  num_events <- rpois(1, lambda * time_horizon)
  
  # Assign revenue for each visit (before or after day 60)
  event_times <- sort(runif(num_events, 0, time_horizon))  # Distribute events uniformly
  revenue <- sum(ifelse(event_times <= coupon_time, coupon_price, price))
  
  # Return the total revenue and the number of visits
  #revenue_rate <- revenue / num_events  # Revenue per visit
  revenue_rate <- revenue / time_horizon
  
  return(revenue_rate)
}



# controls how long the simulation runs for
time_multiplier <- 30
new_time_horizon <- time_horizon*time_multiplier


# coupon characteristics
coupon_time <- 60
price <- 20
discount <- 0.8
coupon_price <- discount*price

# get e(w_1)
# e(w_1)
coupon_time_unscaled <- time_unscaler(coupon_time)
prob_use <- pbeta(q = coupon_time_unscaled, shape1 = alpha, shape2 = beta)
e_w1 <- (coupon_price*prob_use) + (price*(1-prob_use))


e_t <- a + (b-a)*(alpha/(alpha+beta))
expected_lambda <- 1/e_t


# Set parameters
n_years <- 100
time_horizon <- 365 * n_years  # Run for 10 years
num_sources <- 1000  # Keep a reasonable number of sources

# Run the simulation
pooled_events <- superpose_renewal_processes_fixed_time(num_sources, time_horizon)

# Compute interarrival times
pooled_interarrivals <- diff(pooled_events)



# look at the pooled interrarival times
ggplot(data = data.frame(inter=pooled_interarrivals), aes(x=inter)) + 
  geom_histogram(bins = 100) + 
  theme_minimal()+
  labs(x="Interrarival Time",
       y = "Frequency", 
       title = "Time between arrivals in Simulation")
#ggsave(file.path(fig_filepath, "interarrival_frequency_simulation.jpeg"), dpi = 300)


# Estimate event rate (lambda) from pooled renewal process
lambda_pooled <- expected_lambda*num_sources
# Generate Poisson process with the same event rate
poisson_events <- simulate_poisson_process(lambda_pooled, time_horizon)
poisson_interarrivals <- diff(poisson_events)  # Compute interarrival times


saveRDS(pooled_events, file = "sim_n_pooled.rds")
saveRDS(poisson_events, file = "sim_n_pois.rds")
pooled_events <-readRDS(file = "sim_n_pooled.rds")
poisson_events <- readRDS(file = "sim_n_pois.rds")
# Combine data for comparison
df_comparison_1 <- data.frame(
  interarrival_time = c(pooled_interarrivals, poisson_interarrivals),
  process = rep(c("Superposed Renewal", "Poisson"), 
                c(length(pooled_interarrivals), length(poisson_interarrivals)))
)



# next up was the largest simulation, gave the results
n_years <- 100
time_horizon <- 365 * n_years  # Run for 10 years
num_sources <- 1000  # Keep a reasonable number of sources
nsim <- 1000

n_events <- data.frame(
  sim <- c(),
  pois <- c()
)


for(i in 1:nsim){
  # Run the simulation
  #start_time <- Sys.time()  # Start timer
  pooled_events <- superpose_renewal_processes_fixed_time(num_sources, time_horizon)
  # Generate Poisson process with the same event rate
  #poisson_events <- simulate_poisson_process(lambda_pooled, time_horizon)
  n_events <- rbind(n_events, data.frame(
    sim = length(pooled_events),
    pois = rpois(1, lambda_pooled * time_horizon)#length(poisson_events)
  ))
  #end_time <- Sys.time()  # End timer
  #execution_time <- end_time - start_time
  #print(execution_time)  # Prints time difference
}


# Convert to long format
sim_long <- pivot_longer(n_events, cols = c(sim, pois), names_to = "Process", values_to = "Counts")


# Run the simulations
revenue_rates <- replicate(nsim, simulate_total_revenue())

# Convert to dataframe for ggplot
df_revenue <- data.frame(revenue_rate = revenue_rates)

revenue_rates_poisson <- replicate(n_sim, simulate_poisson_revenue(lambda_pooled, time_horizon))

# Convert to dataframe for plotting
df_poisson_revenue <- data.frame(revenue_rate = revenue_rates_poisson)
# Combine both empirical and Poisson revenue rates into one dataframe for comparison
df_comparison <- data.frame(
  revenue_rate = c(revenue_rates, revenue_rates_poisson),
  process = rep(c("Empirical", "Poisson"), c(length(revenue_rates), length(revenue_rates_poisson)))
)

source("CodeForFigures.R")