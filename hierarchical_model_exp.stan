data {
  int<lower=0> N;              // number of observations
  int<lower=1> J;              // number of circuits
  array[N] int circuit_id;     // circuit index for each observation
  vector[N] grid;              // grid position
  vector[N] gap;               // gap to winner in seconds
  int grid_pred;               // grid position to predict at
}

parameters {
  real alpha;                  // global intercept
  real mu_beta;                // mean of circuit slopes
  real<lower=0> sigma_beta;    // sd of circuit slopes
  vector[J] beta_j;            // circuit-specific slopes
}

model {
  // priors
  alpha       ~ normal(-2.5, 1.0);
  mu_beta     ~ normal(-0.5, 0.5);
  sigma_beta  ~ exponential(1);
  beta_j      ~ normal(mu_beta, sigma_beta);

  // likelihood
  for (i in 1:N) {
    real lambda = exp(alpha + beta_j[circuit_id[i]] * log(grid[i]));
    gap[i] ~ exponential(lambda);
  }
}

generated quantities {
  real gap_pred;
  {
    real lambda_pred = exp(alpha + mu_beta * log(grid_pred));
    gap_pred = exponential_rng(lambda_pred);
  }
}