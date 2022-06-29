# This was run on the SciCORE cluster as it handles some quite large data.
library(tidyverse)
library(rstan)

# Get the samples from the model fitting:
sample_files <- file.path('..', 'data', 'saved_objects') %>%
	list.files() %>%
	str_subset('220629') %>%
	{str_c(file.path('..', 'data', 'saved_objects', .))} # nolint
fit_dat <- read_stan_csv(sample_files)

alpha_samples <- rstan::extract(fit_dat, pars = c('hyper_alphas', 'alphas_raw'))

saveRDS(alpha_samples, file.path('..', 'data', 'saved_objects',
	'220617_rl_plus_param_recov_alpha_samples.RDS'))