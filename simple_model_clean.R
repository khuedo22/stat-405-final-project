suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(cmdstanr))

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

# ========= prior check ================
# distribution of data
hist(data$gap)



n_samples <- 100
xs <- seq(1, 20, by = 0.1)

# sample from priors
alpha_prior <- rnorm(n_samples, 0, 5)
beta_prior  <- rnorm(n_samples, 0, 2)

# set up empty plot with range of your actual data
plot(NULL, xlim = c(1, 20), ylim = range(data$gap),
     xlab = "grid", ylab = "gap")

# draw prior curves first
for (i in 1:n_samples) {
  mean_gap <- exp(-(alpha_prior[i] + beta_prior[i] * log(xs)))
  lines(xs, mean_gap, col = rgb(0, 0, 0, alpha = 0.05))
}

# overlay actual data on top
points(data$grid, data$gap, col = "red", pch = 1, cex = 0.8)



# ========= running stan ================
N_obs = nrow(data)
# exponential model
mod_simple_exp = cmdstan_model("simple_model_exp.stan")
fit_simple_exp = mod_simple_exp$sample(
  seed = 1,
  chains = 5,
  refresh = 500,
  output_dir = "stan_out",
  data = list(N = N_obs, 
              grid = data$grid, 
              gap=data$gap, 
              grid_pred=10), # add pred
  iter_warmup = 1000,
  iter_sampling = 1000,
)


fit_exp.variational = mod_simple_exp$variational(
  seed = 1,
  refresh = 500,
  output_dir = "stan_out",
  algorithm = "meanfield",
  output_samples = 1000,
  data = list(N = N_obs, 
              grid = data$grid, 
              gap=data$gap, 
              grid_pred=10)
)

# ========= model summary (ESS and R hat) ================


print(fit_simple_exp)
fit_simple_exp$summary(NULL, c("mean", "sd", "mcse_mean", "ess_bulk"))

print(fit_exp.variational)
fit_exp.variational$summary(NULL, c("mean", "sd", "mcse_mean", "ess_bulk"))

# TODO change this to simple cross validation
# ========= calibration ================
N_train = N_obs-1

ci_level = 0.8
ci_plims = c((1-ci_level)/2, (1+ci_level)/2) # probability limits of the CI



# randomly sample 50 points from the data and set that as the validation since
# there are over 50 data points TODO is this a valid method?

validation_obs <- sample(1:N_obs, 50, replace = FALSE) 
# sample 50 to save time

all_mcmc_samples <- list()
ci_limits_exp = matrix(0, nrow=50, 2)
for(i in 1:length(validation_obs)){
  i_test = validation_obs[i]
  train_test_dta = list(
    N = N_train,
    grid = data$grid[-i_test],
    gap = data$gap[-i_test],
    grid_pred = data$grid[i_test])
  mcmc_results = mod_simple_exp$sample(
    seed = 1,
    data = train_test_dta,
    output_dir = "stan_out",
    refresh       = 0,
    show_messages = FALSE,
    chains = 1
  )
  simple_exp_samples = as.vector(mcmc_results$draws("gap_pred"))
  all_mcmc_samples[[i]] <- as.vector(mcmc_results$draws("gap_pred"))
  ci_limits_exp[i,] = quantile(simple_exp_samples, ci_plims)
}



merged_data_exp = data[validation_obs, ] %>% 
  bind_cols(data.frame(CI_L = ci_limits_exp[,1], CI_R = ci_limits_exp[,2])) %>% 
  mutate(Inside_CI = (gap >= CI_L & gap<= CI_R)) 
merged_data_exp %>% 
  ggplot(aes(x = grid, y = gap, ymin = CI_L, ymax = CI_R, color=Inside_CI)) +
  geom_point() + 
  geom_errorbar() +
  theme_minimal() +
  labs(x = "Grid", y = "gap")

mean(merged_data_exp$Inside_CI)


all_vi_samples <- list()
ci_limits_exp_var = matrix(0, nrow=50, 2)
for(i in 1:length(validation_obs)){
  i_test = validation_obs[i]
  train_test_dta = list(
    N = N_train,
    grid = data$grid[-i_test],
    gap = data$gap[-i_test],
    grid_pred = data$grid[i_test])
  mcmc_results = mod_simple_exp$variational(
    seed = 1,
    data = train_test_dta,
    output_dir = "stan_out",
    refresh       = 0,
    show_messages = FALSE,
    algorithm = "meanfield",
    output_samples = 1000,
  )
  simple_exp_var_samples = as.vector(mcmc_results$draws("gap_pred"))
  all_vi_samples[[i]] <- as.vector(mcmc_results$draws("gap_pred"))
  ci_limits_exp_var[i,] = quantile(simple_exp_var_samples, ci_plims)
}



merged_data_exp_var = data[validation_obs, ] %>% 
  bind_cols(data.frame(CI_L = ci_limits_exp_var[,1], CI_R = ci_limits_exp_var[,2])) %>% 
  mutate(Inside_CI = (gap >= CI_L & gap<= CI_R)) 
merged_data_exp_var %>% 
  ggplot(aes(x = grid, y = gap, ymin = CI_L, ymax = CI_R, color=Inside_CI)) +
  geom_point() + 
  geom_errorbar() +
  theme_minimal() +
  labs(x = "Grid", y = "gap")

mean(merged_data_exp_var$Inside_CI)

# ========= mixing and predictive posterior ================

# trace plots for MH and VI
mcmc_trace(fit_simple_exp$draws("alpha")) + theme_minimal()
mcmc_trace(fit_simple_exp$draws("beta")) + theme_minimal() # this is the parameter of interest

mcmc_trace(fit_exp.variational$draws("alpha")) + theme_minimal()
mcmc_trace(fit_exp.variational$draws("beta")) + theme_minimal()

mcmc_rank_hist(fit_simple_exp$draws("alpha")) + ylab("number of MCMC samples with these ranks") + theme_minimal()

mcmc_rank_hist(fit_simple_exp$draws("beta")) + ylab("number of MCMC samples with these ranks") + theme_minimal()


mcmc_rank_hist(fit_exp.variational$draws("alpha")) + ylab("number of MCMC samples with these ranks") + theme_minimal()

mcmc_rank_hist(fit_exp.variational$draws("beta")) + ylab("number of MCMC samples with these ranks") + theme_minimal()

# posterior predictive check

# parameter values
simple_exp_draws_df <- fit_simple_exp$draws(format = "df")

ggplot(simple_exp_draws_df, 
       aes(x = alpha, y = beta, color = lp__)) +
  geom_point(shape = 1, size = 2) +  # shape=1 gives open circles like your plot
  scale_color_gradient(low = "grey85", high = "purple4") +
  labs(
    x = "alpha parameter",
    y = "beta parameter",
    color = "log posterior"
  ) +
  theme_classic()



# model against data
alpha_samples <- simple_exp_draws_df$alpha
beta_samples <- simple_exp_draws_df$beta

# normalize lp__ to get weights (analogous to norm_weights in your example)
lp <- simple_exp_draws_df$lp__
norm_weights <- exp(lp - max(lp))  # stabilize before normalizing
norm_weights <- norm_weights / sum(norm_weights)

# grid of x values to plot over
xs <- seq(1, 20)

plot(data$grid, data$gap, xlab = "grid", ylab = "gap")

for (i in 1:nrow(simple_exp_draws_df)) {
  alpha <- alpha_samples[i]
  beta  <- beta_samples[i]
  
  # mean of exponential is 1/rate
  mean_gap <- exp(-(alpha + beta * log(xs)))
  
  lines(xs, mean_gap, col = rgb(0, 0, 0, alpha = norm_weights[i] * 20))
}


# TODO maybe do the same thing for VI?


# ========= debugging ================

# calibration on simulated data 

# sample parameters from their priors
# TODO

# parameters
alpha_mean = 0
alpha_sd = 5
beta_mean = 0
beta_sd = 2

simulate_data = function() {
  grid = 1:20
  # sample parameters from their priors
  alpha_sim <- rnorm(1, alpha_mean, alpha_sd)
  beta_sim <- rnorm(1, beta_mean, beta_sd)
  log_grid = log(grid)
  rate = exp(alpha_sim + beta_sim*log_grid)
  
  gap_sim <- as.vector(mapply(function(r) rexp(50, rate = r), rate))
  grid_rep <- rep(1:20, each = 50)
  return(data.frame(alpha = alpha_sim, beta = beta_sim, grid = grid_rep, gap = gap_sim))
  
  
}

simulated_data = simulate_data()

validation_obs_debug <- sample(1:nrow(simulated_data), 100, replace = FALSE)
ci_limits_debug = matrix(0, nrow=100, 2)
# WARNING: this takes a long time to run
for(i in 1:length(validation_obs_debug)){
  i_test = validation_obs_debug[i]
  train_test_debug_dta = list(
    N = nrow(simulated_data)-1,
    grid = simulated_data$grid[-i_test],
    gap = simulated_data$gap[-i_test],
    grid_pred = simulated_data$grid[i_test])
  mcmc_results_debug = mod_simple_exp$sample(
    seed = 1,
    data = train_test_debug_dta,
    output_dir = "stan_out",
    refresh       = 0,
    show_messages = FALSE,
    iter_sampling=500,
    chains = 1
  )
  samples_debug = as.vector(mcmc_results_debug$draws("gap_pred"))
  ci_limits_debug[i,] = quantile(samples_debug, ci_plims)
}

merged_simulated_data = simulated_data[validation_obs_debug, ] %>% 
  bind_cols(data.frame(CI_L = ci_limits_debug[,1], CI_R = ci_limits_debug[,2])) %>% 
  mutate(Inside_CI = (gap >= CI_L & gap <= CI_R)) 
merged_simulated_data %>% 
  ggplot(aes(x = grid, y = gap, ymin = CI_L, ymax = CI_R, color=Inside_CI)) +
  geom_point() + 
  geom_errorbar() +
  theme_minimal() +
  labs(x = "Grid", y = "gap")

mean(merged_simulated_data$Inside_CI)


# ========= exact invarianct test ================



# initial distribution
# 100 samples of alpha, beta for now
initial_dist_df = data.frame()
for (i in 1:100) {
  forward_values = simulate_data()
  initial_dist_df = rbind(initial_dist_df, data.frame(forward_values$alpha[1], forward_values$beta[1]))
}

hist(initial_dist_df$forward_values.beta)

# final distribution
final_dist_df = data.frame()
for (i in 1:100) {
  forward_values = simulate_data()
  dta = list(N = length(forward_values$grid),
             grid = forward_values$grid,
             gap = forward_values$gap,
             grid_pred = 10
  )
  curr_fit = mod_simple_exp$sample(
    seed=1,
    data = dta,
    refresh       = 0,
    show_messages = FALSE,
    output_dir = "stan_out",
    chains=1,
    iter_warmup = 10,
    iter_sampling = 50,
    init = list(list(alpha=forward_values$alpha[1],
                     beta=forward_values$beta[1]))
  )
  final_params <- curr_fit$draws(format = "df") |> 
    tail(1) |>
    subset(select = c(alpha, beta))
  final_dist_df = rbind(final_dist_df, final_params)
}
hist(final_dist_df$beta)

ks.test(initial_dist_df$forward_values.beta, final_dist_df$beta)

hist(initial_dist_df$forward_values.beta)
hist(final_dist_df$beta)

# ========= MCMC vs VI ================

# compare credible intervals for mcmc and vi
merged_data_exp$method = "mcmc"
merged_data_exp_var$method = "vi"


combined_merged_data <- bind_rows(merged_data_exp, merged_data_exp_var)



combined_merged_data %>%
  ggplot(aes(x = grid, y = gap, ymin = CI_L, ymax = CI_R, color = Inside_CI)) +
  geom_errorbar(width = 0.3, alpha = 0.7) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c("TRUE" = "#2196F3", "FALSE" = "#F44336"),
    labels = c("TRUE" = "Inside CI", "FALSE" = "Outside CI")
  ) +
  facet_wrap(~ method, ncol = 1) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    legend.title      = element_blank(),
    strip.text        = element_text(face = "bold", size = 11),
    panel.grid.minor  = element_blank()
  ) +
  labs(title = "Posterior Predictive Credible Intervals",
       x = "Grid", y = "Gap")

# plot predicted gap vs actual gap for both methods 

gap_preds_df = data.frame(mcmc=simple_exp_samples,
                          vi=simple_exp_var_samples,
                          true_preds = data[validation_obs, ]$gap)

gap_preds_df %>% select(mcmc, vi) %>%
  pivot_longer(cols = everything(), names_to = "method", values_to = "gap_preds") %>%
  ggplot(aes(x = gap_preds, fill = method)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  theme_minimal()

gap_preds_df %>% 
  pivot_longer(cols = c(mcmc, vi), names_to = "method", values_to = "predicted") %>%
  ggplot(aes(x = true_preds, y = predicted, color = method)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  theme_minimal()

gap_preds_df %>%
  pivot_longer(cols = c(mcmc, vi), names_to = "method", values_to = "predicted") %>%
  ggplot(aes(x = true_preds, y = predicted, color = method)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  coord_fixed(xlim = range(gap_preds_df$true_preds), ylim = range(gap_preds_df$true_preds)) +
  facet_wrap(~ method) +
  theme_minimal()