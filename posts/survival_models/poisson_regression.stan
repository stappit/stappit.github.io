data {
  int<lower = 1> n;                // length of dataframe
  int<lower = 1> m;                // # intervals
  
  int<lower = 0, upper = 1> death[n];         // deaths of the i-th case
  vector<lower = 0, upper = 1>[n] metastized; //
  vector<lower = 1>[n] exposure;   //
  
  matrix<lower = 0, upper = 1>[n, m] I;
  
  real<lower = 0> h_sh;
  real<lower = 0> h_ra;
  real<lower = 0> beta_sigma;
}

transformed data {
  vector[n] log_exposure = log(exposure);
}

parameters {
  real beta;
  vector<lower = 0>[m] h;
}

transformed parameters {
  vector<lower = 0>[n] mu = (I * h) .* exp(metastized * beta + log_exposure);
}

model {
  h ~ gamma(h_sh, h_ra);
  beta ~ normal(0, beta_sigma);
  death ~ poisson(mu);
}
