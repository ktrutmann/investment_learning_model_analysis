# Resources:
# https://vasishth.github.io/bayescogsci/book/model-comparison-in-stan.html#fn28

library(rstan)
library(bridgesampling)
library(loo)
library(tidyverse)

saved_objects_path <- file.path('..', 'data', 'saved_objects')

rl_plus_model <- readRDS(file.path(saved_objects_path, '210127_rl_plus_main_study.RDS'))
rl_simple_model <- readRDS(file.path(saved_objects_path, '210127_rl_simple_main_study.RDS'))

# Avoiding this error: https://github.com/quentingronau/bridgesampling/issues/7
stan_dat <- readRDS(file.path(saved_objects_path, 'stan_dat.RDS'))
multi_alpha_dummy <- stan(file = file.path('..', 'model_fitting',
		'models', 'multi_alpha_rl.stan'),
	data = stan_dat,
	chains = 0)
simple_alpha_dummy <- stan(file = file.path('..', 'model_fitting',
		'models', 'single_alpha_rl.stan'),
	data = stan_dat,
	chains = 0)


# Bayes Factors ---------------------------------------
lml_rl_plus <- bridge_sampler(samples = rl_plus_model,
	stanfit_model = multi_alpha_dummy,
    repetitions = 1, maxiter = 100, silent = FALSE,
  	verbose = TRUE)
lml_rl_simple <- bridge_sampler(samples = rl_simple_model,
	stanfit_model = simple_alpha_dummy)
(bf_rl_models <- bridgesampling::bf(lml_rl_plus, lml_rl_simple))

# LOO evaluation ----------------------------------------------------
log_lik_rl_plus <- extract_log_lik(rl_plus_model, merge_chains = FALSE)

loo_rl_plus <- loo::loo(log_lik_rl_plus,
print(loo_rl_plus)
 r_eff_rl_plus <- relative_eff(exp(log_lik_rl_plus), cores = 4)

loo_rl_plus <- loo::loo(log_lik_rl_plus,
	r_eff = r_eff_rl_plus, cores = 4)
 saveRDS(loo_rl_plus, file.path(saved_objects_path, 'loo_rl_plus.RDS'))
print(loo_rl_plus)

