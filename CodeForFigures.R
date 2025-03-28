
# generate a plot of the probability of getting a haircut
# Figure 1 in paper
ggplot(data.frame(x = seq(a, b, length.out = 100)), aes(x)) + 
  stat_function(fun = function(x) (prob_cut_at_t(x)),
                color = "blue") +
  labs(x = "Time of Haircut", y ="pdf", title = "Probability of Haircut Times")+
  theme_minimal() + 
  geom_vline(xintercept = coupon_time, col="red",linetype="dashed")  +
  annotate("text", 
           x = coupon_time + 2, y = 0.03, 
           label = "Coupon Time", color = "red", angle = 0, hjust = 0)

ggsave(file.path(fig_filepath, "haircut_time_probability_with_coupon.jpeg"), dpi = 300)



# Generate data for different values of c
different_coupon_times <- expand.grid(
  t = seq(a, b, length.out = 500),
  c = c(0.5,0.75,1.0,1.25,1.5,1.75,2.0)
)

# Compute probability for each (t, c) pair
different_coupon_times$prob <- mapply(
  prob_cut_at_t_new_coupon,
  different_coupon_times$t,
  different_coupon_times$c
)

# Plot
# Figure 2 in paper
ggplot(different_coupon_times, aes(x = t, y = prob, color = factor(c))) +
  geom_line(size = 1) +
  labs(
    x = "Time of Haircut",
    y = "PDF",
    title = "Probability of Haircut Times for Different Coupon Values",
    color = "Coupon Scaling Factor (c)"
  ) +
  theme_minimal()

ggsave(file.path(fig_filepath, "c_shifts_time.jpeg"), dpi = 300)



# didn't use the next few figures in the paper
# Histogram comparison
ggplot(df_comparison_1, aes(x = interarrival_time, fill = process)) +
  geom_histogram(alpha = 0.5, bins = 50, position = "identity") +
  #scale_x_log10() +  # Log scale for better visualization
  labs(title = "Comparison of Interarrival Times: Superposed vs. Poisson",
       x = "Interarrival Time", y = "Frequency") +
  theme_minimal()



# Density plot comparison
ggplot(df_comparison_1, aes(x = interarrival_time, fill = process)) +
  geom_density(alpha = 0.5, color = NA) +
  # scale_x_log10() +  # Log scale for better visualization
  labs(title = "Comparison of Interarrival Times: Superposed vs. Poisson",
       x = "Interarrival Time (log scale)", y = "Density") +
  theme_minimal()




# Plot histogram
ggplot(sim_long, aes(x = Counts, fill = Process)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +  # Overlapping histograms
  labs(title = "Comparison of Total Visits: Superposed vs. Poisson",
       subtitle = paste0(nsim," simulations, over ", n_years," years with ", num_sources," sources"),
       x = "Number of Visits", y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values=c("sim"="blue","pois"="red"),labels = c("Poisson", "Simulation"))  
ggsave(file.path(fig_filepath, "number_visits_compared.jpeg"), dpi = 300)
# Plot histogram of Poisson revenue rates
ggplot(df_poisson_revenue, aes(x = revenue_rate)) +
  geom_histogram(fill = "blue", alpha = 0.5, bins = 30) +
  labs(title = "Poisson Process Empirical Revenue Rate Distribution",
       subtitle = paste0(num_sources," customers, ", n_sim,", simulations over ", n_years, " years"),
       x = "Revenue per Visit", y = "Frequency") +
  theme_minimal()
ggsave(file.path(fig_filepath, "poisson_revenue_rates.jpeg"), dpi = 300)

# Plot histogram of empirical revenue rates
ggplot(df_revenue, aes(x = revenue_rate)) +
  geom_histogram(fill = "blue", alpha = 0.5, bins = 30) +
  labs(title = "Empirical Revenue Rate Distribution",       
       subtitle = paste0(num_sources," customers, ", n_sim,", simulations over ", n_years, " years"),
       x = "Revenue per Visit", y = "Frequency") +
  theme_minimal()
ggsave(file.path(fig_filepath, "empirical_revenue_rates.jpeg"), dpi = 300)




# Plot histogram to compare empirical and Poisson revenue rates
ggplot(df_comparison, aes(x = revenue_rate, fill = process)) +
  geom_histogram(alpha = 0.5, bins = 40, position = "identity") +
  labs(title = "Simulations of Empirical vs Poisson Revenue Rates",
       subtitle = paste0(num_sources," customers, ", n_sim,", simulations over ", n_years, " years"),
       x = "Revenue per Visit", y = "Frequency") +
  theme_minimal()  +scale_fill_manual(values=c("Empirical"="blue","Poisson"="red"),labels = c("Beta", "Poisson"))  

ggsave(file.path(fig_filepath, "revenue_rates_simulations.jpeg"), dpi = 300)

