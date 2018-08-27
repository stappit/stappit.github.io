data {
  int<lower = 0> n;
  real y;
}

parameters {
  real theta;
}

model {
  theta ~ normal(180, 40);
  for (i in 1:n)
    y ~ normal(theta, 20);
}
