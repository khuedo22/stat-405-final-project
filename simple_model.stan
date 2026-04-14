//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] grid;
  vector[N] gap;
  int<lower=1, upper=20> grid_pred;
}

transformed data {
  vector[N] log_gap = log(1 + gap);
}
// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha;
  real mu_beta;
  real<lower=0> sigma_beta;
  real beta;
  real<lower=0> sigma;
}


model {
  alpha ~ normal(2, 1);
  mu_beta ~ normal(0.5, 0.5);
  sigma_beta ~ exponential(1);
  beta ~ normal(mu_beta, sigma_beta);
  sigma ~ exponential(1);
  log_gap ~ normal(alpha + beta * grid, sigma);
}

generated quantities {
  real log_gap_pred = normal_rng(alpha + beta*grid_pred, sigma);
}
