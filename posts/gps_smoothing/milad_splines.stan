data {
  int<lower = 1> n;
  vector<lower = 0>[n] hour;
  vector<lower = 0>[n] km; 
  vector<lower = 0>[n] cadence; 
  int<lower = 1> k;
  matrix[n, k] B;
  
  real<lower = 0> b0_scale;
  real<lower = 0> tau_scale;
  real<lower = 0> sigma_scale;
}

transformed data {
  vector<lower = 0>[n] kph;
  vector<lower = 0>[n] km_delta; 
  vector<lower = 0>[n] hour_delta; 
  
  kph[1] = 0;
  hour_delta[1] = 0;
  km_delta[1] = 0;
  
  for (t in 2:n) {
    hour_delta[t] = hour[t] - hour[t - 1];
    km_delta[t] = km[t] - km[t - 1];
    kph[t] = km_delta[t] / hour_delta[t];
  }
}

parameters {
  real<lower = 0> v;
  vector[k] b0; 
  real<lower = 0> tau;
  real<lower = 0> sigma;
}

transformed parameters {
  vector[k] b; 
  vector<lower = 0>[n] mu;
  vector<lower = 0>[n] rate;
  vector<lower = 0>[n] shape;
  
  b[1] = b0[1];
  for (t in 2:k)
    b[t] = b[t - 1] + b0[t] * tau;
  
  mu = v * exp(B * b);
  rate = mu / (sigma^2);
  shape =  rate .* mu;
}

model {
  v ~ gamma(4, 0.33333);
  sigma ~ normal(0, sigma_scale);
  tau ~ normal(0, tau_scale);
  b0 ~ normal(0, b0_scale);
  
  kph ~ gamma(shape, rate);
}
