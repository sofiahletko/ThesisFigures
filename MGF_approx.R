set.seed(24)
library(dplyr)
library(ggplot2)
library(tidyverse)
fig_filepath <- "~/Documents/Duke-Classes/IndependentStudy/github-repo/RFigures"

pooled_events <-readRDS(file = "sim_n_pooled.rds")
poisson_events <- readRDS(file = "sim_n_pois.rds")
poisson_interarrivals <- diff(poisson_events)  # Compute interarrival times
pooled_interarrivals <- diff(pooled_events)

s = 1000
samples <- data.frame(
  pooled = sample(pooled_interarrivals, replace = TRUE, size = s),
  pois = sample(poisson_interarrivals, replace = TRUE, size = s)
)


t_vals <- seq(-1, 10, length.out = 200)

# Compute empirical MGFs
mgf_pooled <- sapply(t_vals, function(t) mean(exp(t * samples$pooled)))
mgf_pois   <- sapply(t_vals, function(t) mean(exp(t * samples$pois)))

mgf_df <- data.frame(t = t_vals, 
                     pooled = mgf_pooled, 
                     pois = mgf_pois) |>
  pivot_longer(cols = c("pooled", "pois"), names_to = "Type", values_to = "MGF")|>
  mutate(Type = recode(Type,
                       pois = "Poisson",
                       pooled = "Pooled Events"))

ggplot(data = mgf_df, aes(x=t, y = MGF, colour = Type))+geom_line()+
  scale_color_manual(values = c("pooled" = "blue", "pois" = "red"))


ggplot(mgf_df, aes(x = t, y = MGF, colour = Type,linetype = Type)) +
  geom_line() +
  labs(title = "Empirical MGF of Interarrival Times",
       x = "t", y = "MGF") +
  theme_minimal() +scale_color_manual(values = c("Pooled Events" = "blue", "Poisson" = "red")) +
  scale_linetype_manual(values = c("Pooled Events" = "solid", "Poisson" = "dashed"))

