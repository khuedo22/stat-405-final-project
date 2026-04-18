data {
  int<lower=0> N;
  vector[N] grid;
  vector[N] gap;
  int<lower=1, upper=20> grid_pred;
}


// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha;
  real beta;
}

transformed parameters {
  vector[N] log_grid = log(grid);
  real log_grid_pred = log(grid_pred);
}

model {
  alpha ~ normal(0, 5);
  beta ~ normal(0, 2);
  
  for (n in 1:N) {
    target += exponential_lpdf(gap[n] | exp(alpha + beta * log_grid[n]));
  }
}

generated quantities {
  real gap_pred = exponential_rng(exp(alpha + beta*log_grid_pred));
}

