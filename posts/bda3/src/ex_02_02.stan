data {
  real<lower = 0> theta_alpha;
  real<lower = 0> theta_beta;
}

parameters {
  // theta = probability of coin 1
  real<lower = 0, upper = 1> theta;
}

model {
  theta ~ beta(theta_alpha, theta_beta); // uniform prior 
  target += log_mix(theta, 
                    binomial_lpmf(0 | 2, 0.6), 
                    binomial_lpmf(0 | 2, 0.4)
                   );
}
