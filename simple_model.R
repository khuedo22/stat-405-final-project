suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(cmdstanr))
source("simple.R")
source("simple_utils.R")

# ========= load and preprocess data ================
data <- read.csv("full_data.csv")
colnames(data)

convert_to_seconds <- function(x) {
  if (is.numeric(x)) return(x)
  
  x <- as.character(x)
  
  sapply(x, function(val) {
    # Plain number (no colon)
    if (grepl("^[+-]?\\d+\\.?\\d*$", val)) {
      return(as.numeric(val))
    }
    
    # No sign prefix and contains a colon = gap of 0
    if (!grepl("^[+-]", val) && grepl(":", val)) {
      return(0)
    }
    
    # Extract sign
    sign <- ifelse(startsWith(val, "-"), -1, 1)
    val <- gsub("^[+-]", "", val)
    
    parts <- strsplit(val, ":")[[1]]
    
    if (length(parts) == 2) {
      minutes <- as.numeric(parts[1])
      seconds <- as.numeric(parts[2])
      return(sign * (minutes * 60 + seconds))
    } else if (length(parts) == 3) {
      hours   <- as.numeric(parts[1])
      minutes <- as.numeric(parts[2])
      seconds <- as.numeric(parts[3])
      return(sign * (hours * 3600 + minutes * 60 + seconds))
    } else {
      return(NA)
    }
  }, USE.NAMES = FALSE)
}
data <- data %>%
  mutate(gap = convert_to_seconds(gap), 
         grid = as.integer(grid)) %>%
  filter(
    status == "Finished",       # only finishers
    !is.na(grid),
    grid > 0,                    # exclude pit lane starts (grid = 0)
    !is.na(gap))




set.seed(1)

# ========= parameters ================
# normal
alpha_mean = 2
alpha_sd = 1
mu_beta_mean = 0.5
mu_beta_sd = 0.5
sigma_beta_lambda = 1
sigma_lambda = 1


# ========= prior check ================

# normal model with log transformation
plot(data$grid, log(data$gap + 1), ylab = "log(Gap + 1)", xlab = "Grid Position")
for (i in 1:50) {
  intercept = simulate(Norm(alpha_mean, alpha_sd))
  slope     = simulate(Norm(rnorm(1, mu_beta_mean, mu_beta_sd), 
                            rexp(1, sigma_beta_lambda)))
  lines(intercept + slope * 1:20, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
}



# normal model without log transformation
set.seed(1)
plot(data$grid, data$gap, ylab = "Gap", xlab = "Grid Position")
for (i in 1:50) {
  intercept = simulate(Norm(15, 15))
  slope     = simulate(Norm(5, 4))
  lines(intercept + slope * 1:20, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
}


# create directory for stan file
dir.create(file.path("stan_out"), showWarnings = FALSE)


N_obs = nrow(data)

# ========= running stan ================

# first attempt
mod_simple = cmdstan_model("simple_model.stan")
fit_simple = mod_simple$sample(
  seed = 1,
  chains = 5,
  refresh = 500,
  output_dir = "stan_out",
  data = list(N = N_obs, grid = data$grid, gap=data$gap, grid_pred=10), # add pred
  iter_warmup = 1000,
  iter_sampling = 1000,
)

print(fit_simple)
fit_simple$summary(NULL, c("mean", "sd", "mcse_mean", "ess_bulk"))




# gamma model (second attempt)
mod_gamma = cmdstan_model("simple_model_gamma.stan")

fit_gamma = mod_gamma$sample(
  seed = 1,
  chains = 5,
  refresh = 500,
  output_dir = "stan_out",
  data = list(N = N_obs, grid = data$grid, gap=data$gap, grid_pred=10), # add pred
  iter_warmup = 1000,
  iter_sampling = 1000,
)

print(fit_gamma)
fit_gamma$summary(NULL, c("mean", "sd", "mcse_mean", "ess_bulk"))





# ========= calibration ================
N_train = N_obs-1

ci_level = 0.8
ci_plims = c((1-ci_level)/2, (1+ci_level)/2) # probability limits of the CI
ci_limits = matrix(0, nrow=50, 2)


# randomly sample 50 points from the data and set that as the validation since
# there are over 1000 data points TODO is this a valid method?
set.seed(1)
validation_obs <- sample(1:N_obs, 50, replace = FALSE)

# first attempt
ci_limits_simple = matrix(0, nrow=50, 2)
for(i in 1:length(validation_obs)){
  i_test = validation_obs[i]
  train_test_dta = list(
    N = N_train,
    grid = data$grid[-i_test],
    gap = data$gap[-i_test],
    grid_pred = data$grid[i_test])
  mcmc_results = mod_simple$sample(
    seed = 1,
    data = train_test_dta,
    output_dir = "stan_out",
    chains = 1
  )
  samples = as.vector(mcmc_results$draws("log_gap_pred"))
  ci_limits_simple[i,] = quantile(samples, ci_plims)
}



merged_data_simple = data[validation_obs, ] %>% 
  bind_cols(data.frame(CI_L = ci_limits_simple[,1], CI_R = ci_limits_simple[,2])) %>% 
  mutate(Inside_CI = (log(1+gap) >= CI_L & log(1+gap) <= CI_R)) 
merged_data_simple %>% 
  ggplot(aes(x = grid, y = log(gap+1), ymin = CI_L, ymax = CI_R, color=Inside_CI)) +
  geom_point() + 
  geom_errorbar() +
  theme_minimal() +
  labs(x = "Grid", y = "Log(gap+1")

mean(merged_data_simple$Inside_CI)


# gamma
ci_limits_gamma = matrix(0, nrow=50, 2)
for(i in 1:length(validation_obs)){
  i_test = validation_obs[i]
  train_test_dta = list(
    N = N_train,
    grid = data$grid[-i_test],
    gap = data$gap[-i_test],
    grid_pred = data$grid[i_test])
  mcmc_results = mod_gamma$sample(
    seed = 1,
    data = train_test_dta,
    output_dir = "stan_out",
    chains = 1
  )
  samples = as.vector(mcmc_results$draws("gap_pred"))
  ci_limits_gamma[i,] = quantile(samples, ci_plims)
}

merged_data_gamma = data[validation_obs, ] %>% 
  bind_cols(data.frame(CI_L = ci_limits_gamma[,1], CI_R = ci_limits_gamma[,2])) %>% 
  mutate(Inside_CI = gap >= CI_L & gap <= CI_R)
merged_data_gamma %>% 
  ggplot(aes(x = grid, y = gap, ymin = CI_L, ymax = CI_R, color=Inside_CI)) +
  geom_point() + 
  geom_errorbar() +
  theme_minimal() +
  labs(x = "Grid", y = "gap")

mean(merged_data_gamma$Inside_CI)





# ========= mixing ================

# first attempt
mcmc_trace(fit_simple$draws("alpha")) + theme_minimal()
mcmc_trace(fit_simple$draws("beta")) + theme_minimal() # this is the parameter of interest

mcmc_rank_hist(fit_simple$draws("beta")) + ylab("number of MCMC samples with these ranks") + theme_minimal()

# gamma
mcmc_trace(fit_gamma$draws("alpha")) + theme_minimal()
mcmc_trace(fit_gamma$draws("beta")) + theme_minimal()

mcmc_rank_hist(fit_gamma$draws("beta")) + ylab("number of MCMC samples with these ranks") + theme_minimal()






# ========= debugging ================

# calibration on simulated data 

# sample parameters from their priors
simulate_data = function() {
  grid = 1:20
  # sample parameters from their priors
  alpha_sim <- rnorm(1, alpha_mean, alpha_sd)
  mu_beta_sim <- rnorm(1, mu_beta_mean, mu_beta_sd)
  sigma_beta_sim <- exp(sigma_beta_lambda)
  beta_sim  <- rnorm(1, mu_beta_sim, sigma_beta_sim)
  sigma_sim <- rexp(1, 1)
  
  # use your actual grid values from your data
  # simulate log(1 + gap) ~ N(alpha + beta * grid, sigma)
  log_gap_sim <- rnorm(n = length(grid), mean = alpha_sim + beta_sim * grid, 
                       sd = sigma_sim)
  
  # back-transform to original scale
  gap_sim <- exp(log_gap_sim) - 1
  
  
  return(data.frame(grid=grid, gap=gap_sim))
}


# gamma
simulate_data_gamma = function() {
  # sample parameters from their priors
  grid = 1:20
  alpha_sim <- rnorm(1, 15, 15)
  beta_sim  <- rnorm(1, 5, 4)
  rate <- rexp(1, 1)
  
  # use your actual grid values from your data
  # simulate log(1 + gap) ~ N(alpha + beta * grid, sigma)
  gap_sim <- rgamma(n = length(grid), shape = exp(alpha_sim + beta_sim * grid), 
                       rate = rate)
  
  # back-transform to original scale
  
  
  
  return(data.frame(grid=grid, gap=gap_sim))
}



# first attempt
simulated_data = data.frame()
for (i in 1:50) {
  simulated_data = rbind(simulated_data, simulate_data())
}
# TODO for now, set any values less than 0 to be 0, might not be valid
simulated_data = simulated_data %>%
  mutate(gap = pmax(0, gap))


debug_fit = mod_simple$sample(
  seed = 1,
  chains = 5,
  refresh = 500,
  output_dir = "stan_out",
  data = list(N = nrow(simulated_data), 
              grid = simulated_data$grid, gap=simulated_data$gap, 
              grid_pred=10), # add pred
  iter_warmup = 1000,
  iter_sampling = 1000,
)

print(debug_fit)



set.seed(1)
validation_obs_debug <- sample(1:nrow(simulated_data), 50, replace = FALSE)
ci_limits_debug = matrix(0, nrow=50, 2)

for(i in 1:length(validation_obs_debug)){
  i_test = validation_obs_debug[i]
  train_test_debug_dta = list(
    N = nrow(simulated_data)-1,
    grid = simulated_data$grid[-i_test],
    gap = simulated_data$gap[-i_test],
    grid_pred = simulated_data$grid[i_test])
  mcmc_results_debug = mod_simple$sample(
    seed = 1,
    data = train_test_debug_dta,
    output_dir = "stan_out",
    chains = 1
  )
  samples_debug = as.vector(mcmc_results_debug$draws("log_gap_pred"))
  ci_limits_debug[i,] = quantile(samples_debug, ci_plims)
}

merged_simulated_data = simulated_data[validation_obs_debug, ] %>% 
  bind_cols(data.frame(CI_L = ci_limits_debug[,1], CI_R = ci_limits_debug[,2])) %>% 
  mutate(Inside_CI = (log(1+gap) >= CI_L & log(1+gap) <= CI_R)) 
merged_simulated_data %>% 
  ggplot(aes(x = grid, y = log(gap+1), ymin = CI_L, ymax = CI_R, color=Inside_CI)) +
  geom_point() + 
  geom_errorbar() +
  theme_minimal() +
  labs(x = "Grid", y = "Log(gap+1")

mean(merged_simulated_data$Inside_CI)




# gamma attempt

simulated_data_gamma = data.frame()
for (i in 1:50) {
  simulated_data_gamma = rbind(simulated_data_gamma, simulate_data_gamma())
}


debug_fit_gamma = mod_gamma$sample(
  seed = 1,
  chains = 5,
  refresh = 500,
  output_dir = "stan_out",
  data = list(N = nrow(simulated_data_gamma), 
              grid = simulated_data_gamma$grid, gap=simulated_data_gamma$gap, 
              grid_pred=10), # add pred
  iter_warmup = 1000,
  iter_sampling = 1000,
)

set.seed(1)
validation_obs_debug_gamma <- sample(1:nrow(simulated_data_gamma), 50, replace = FALSE)
ci_limits_debug_gamma = matrix(0, nrow=50, 2)

for(i in 1:length(validation_obs_debug)){
  i_test = validation_obs_debug[i]
  train_test_debug_dta = list(
    N = nrow(simulated_data)-1,
    grid = simulated_data$grid[-i_test],
    gap = simulated_data$gap[-i_test],
    grid_pred = simulated_data$grid[i_test])
  mcmc_results_debug = mod_gamma$sample(
    seed = 1,
    data = train_test_debug_dta,
    output_dir = "stan_out",
    chains = 1
  )
  samples_debug = as.vector(mcmc_results_debug$draws("gap_pred"))
  ci_limits_debug_gamma[i,] = quantile(samples_debug, ci_plims)
}

merged_simulated_data_gamma = simulated_data_gamma[validation_obs_debug_gamma, ] %>% 
  bind_cols(data.frame(CI_L = ci_limits_debug[,1], CI_R = ci_limits_debug[,2])) %>% 
  mutate(Inside_CI = gap >= CI_L & gap <= CI_R)
merged_simulated_data_gamma %>% 
  ggplot(aes(x = grid, y = gap, ymin = CI_L, ymax = CI_R, color=Inside_CI)) +
  geom_point() + 
  geom_errorbar() +
  theme_minimal() +
  labs(x = "Grid", y = "gap")

mean(merged_simulated_data_gamma$Inside_CI)




# ========= exact invarianct test ================
forward = function(groups) {
  alpha = rnorm(1, alpha_mean, alpha_sd)
  mu_beta_sim <- rnorm(1, mu_beta_mean, mu_beta_sd)
  sigma_beta_sim <- rexp(1, sigma_beta_lambda)
  beta  <- rnorm(1, mu_beta_sim, sigma_beta_sim)
  sigma = rexp(1, sigma_lambda)
  grid = 1:20 
  gap = rep(0, groups*length(grid))
  for (group in 1:groups) {
    gaps = exp(rnorm(length(grid), alpha + beta * grid, sigma))-1
    start_idx = (group-1) * length(grid)+1
    end_idx = group * length(grid)
    gap[start_idx:end_idx] = pmax(gaps, 0)
  }
  
  return(
    list(
      alpha = alpha,
      mu_beta = mu_beta_sim,
      sigma_beta = sigma_beta_sim,
      beta = beta,
      sigma = sigma,
      grid=rep(grid, groups),
      gap = gap
    ))
}

# initial distribution
initial_dist_df = data.frame()
for (i in 1:500) {
  forward_values = forward(2)
  initial_dist_df = rbind(initial_dist_df, data.frame(forward_values$alpha, forward_values$beta, forward_values$sigma))
}

hist(initial_dist_df$forward_values.beta)

# final distribution
final_dist_df = data.frame()
for (i in 1:500) {
  forward_values = forward(2)
  dta = list(N = length(forward_values$grid),
            grid = forward_values$grid,
             gap = forward_values$gap,
            grid_pred = 10
             )
  curr_fit = mod_simple$sample(
    seed=1,
    data = dta,
    refresh       = 0,
    show_messages = FALSE,
    output_dir = "stan_out",
    chains=1,
    iter_warmup = 50,
    iter_sampling = 100,
    init = list(list(alpha=forward_values$alpha,
                     mu_beta = forward_values$mu_beta,
                     sigma_beta = forward_values$sigma_beta,
                     beta=forward_values$beta, 
                     sigma=forward_values$sigma))
  )
  final_params <- curr_fit$draws(format = "df") |> 
    tail(1) |>
    subset(select = c(alpha, beta, sigma))
  final_dist_df = rbind(final_dist_df, final_params)
}
hist(final_dist_df$beta)

ks.test(initial_dist_df$forward_values.beta, final_dist_df$beta)

hist(initial_dist_df$forward_values.beta)
hist(final_dist_df$beta)


 



# ess

