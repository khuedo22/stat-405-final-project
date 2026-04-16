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


model {
  alpha ~ normal(0, 50);
  beta ~ normal(0, 10);
  gap ~ exponential(exp(alpha + beta * grid));
}

generated quantities {
  real gap_pred = exponential_rng(exp(alpha + beta*grid_pred));
}

