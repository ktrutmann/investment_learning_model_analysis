# This was run on the SciCORE cluster as it handles some quite large data.
library(tidyverse)
library(rstan)

# Get the samples from the model fitting:
fit_dat <- readRDS(
	file.path('..', 'data', 'saved_objects',
		'220617_rl_plus_param_recov.RDS'))

alpha_samples <- rstan::extract(fit_dat, pars = c('hyper_alphas', 'alphas_raw'))

saveRDS(alpha_samples, file.path('..', 'data', 'saved_objects',
	'220617_rl_plus_param_recov_alpha_samples.RDS'))