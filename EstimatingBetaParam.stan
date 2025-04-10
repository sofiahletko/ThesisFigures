data {
  int<lower=1> N;
  array[N] real<lower=0, upper=1> y;}
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
}
model {
  y ~ beta(alpha, beta);
  // assumes uniform prior if leaving blank
  // these priors assume people wait closer to the end
  // of the window
  alpha ~ gamma(2, 1); 
  beta  ~ gamma(1, 1); 
}

