data {
  int<lower=0> N;
  int<lower=1> J;
  array[N] int circuit_id;
  vector[N] grid;
  vector[N] gap;
  int grid_pred;
  int<lower=1, upper=J> circuit_pred;
}

parameters {
  real alpha;
  real mu_beta;
  real<lower=0> sigma_beta;
  vector[J] beta_j;
}

model {
  alpha      ~ normal(-2.5, 1.0);
  mu_beta    ~ normal(-0.5, 0.5);
  sigma_beta ~ exponential(1);
  beta_j     ~ normal(mu_beta, sigma_beta);

  for (i in 1:N) {
    real lambda = exp(alpha + beta_j[circuit_id[i]] * log(grid[i]));
    gap[i] ~ exponential(lambda);
  }
}

generated quantities {
  real gap_pred;
  {
    real lambda_pred = exp(alpha + beta_j[circuit_pred] * log(grid_pred));
    gap_pred = exponential_rng(lambda_pred);
  }
}