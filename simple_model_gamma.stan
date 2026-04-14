data {
  int<lower=0> N;
  vector[N] grid;
  vector[N] gap;
  int<lower=1, upper=20> grid_pred;
}


parameters {
  real alpha;
  real beta;
  real<lower=0> rate;
}


model {
  alpha ~ normal(15, 15);
  beta ~ normal(5, 4);
  rate ~ exponential(1);
  gap ~ gamma(exp(alpha + beta * grid), rate);
}

generated quantities {
  real gap_pred = gamma_rng(exp(alpha + beta*grid_pred), rate);
}


