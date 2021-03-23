# Resources:
# https://vasishth.github.io/bayescogsci/book/model-comparison-in-stan.html#fn28

library(rstan)
library(bridgesampling)
library(tidyverse)

options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')


saved_objects_path <- file.path('..', 'saved_objects')

rl_plus_model <- readRDS(file.path(saved_objects_path, '210127_rl_plus_main_study.RDS'))
rl_simple_model <- readRDS(file.path(saved_objects_path, '210127_rl_simple_main_study.RDS'))


rl_invested_model <- read_stan_csv(file.path(saved_objects_path,
	str_c('210310_samples_rl_invested_main_study_', 1:4, '.csv')))
rl_returns_model <- read_stan_csv(file.path(saved_objects_path,
	str_c('210311_rl_returns_main_study_', 1:4, '.csv')))
rl_imoves_model <- read_stan_csv(file.path(saved_objects_path,
	str_c('210318_rl_moves_main_study_', 1:4, '.csv')))


# Avoiding this error: https://github.com/quentingronau/bridgesampling/issues/7
stan_dat <- readRDS(file.path(saved_objects_path, 'stan_dat.RDS'))
multi_alpha_dummy <- stan(file = file.path('..',
	'investment_learning_model_fitting', 'models',
	'multi_alpha_rl.stan'), data = stan_dat, chains = 0)
simple_alpha_dummy <- stan(file = file.path('..',
	'investment_learning_model_fitting', 'models', 'single_alpha_rl.stan'),
	data = stan_dat, chains = 0)
invested_alpha_dummy <- stan(file = file.path('..',
	'investment_learning_model_fitting', 'models',
	'multi_alpha_invested_rl.stan'), data = stan_dat, chains = 0)
moves_alpha_dummy <- stan(file = file.path('..',
	'investment_learning_model_fitting', 'models',
	'multi_alpha_moves_rl.stan'), data = stan_dat, chains = 0)
returns_alpha_dummy <- stan(file = file.path('..',
	'investment_learning_model_fitting', 'models',
	'multi_alpha_returns_rl.stan'), data = stan_dat, chains = 0)



lml_rl_invested <- bridge_sampler(samples = rl_invested_model,
	stanfit_model = invested_alpha_dummy,
	repetitions = 2, maxiter = 500, verbose = TRUE, cores = 4)
saveRDS(lml_rl_invested, file.path(saved_objects_path, 'sms_rl_invested.RDS'))

rm(lml_rl_invested)

lml_rl_moves <- bridge_sampler(samples = rl_moves_model,
	stanfit_model = moves_alpha_dummy,
	repetitions = 5, maxiter = 1000, verbose = TRUE, cores = 4)
saveRDS(lml_rl_moves, file.path(saved_objects_path, 'sms_rl_moves.RDS'))
# lml_rl_moves <- readRDS(file.path(saved_objects_path, 'sms_rl_moves.RDS'))

rm(lml_rl_moves)

lml_rl_returns <- bridge_sampler(samples = rl_returns_model,
	stanfit_model = returns_alpha_dummy,
	repetitions = 5, maxiter = 1000, verbose = TRUE, cores = 4)
saveRDS(lml_rl_returns, file.path(saved_objects_path, 'sms_rl_returns.RDS'))

(bf_rl_models <- bridgesampling::bf(lml_rl_plus, lml_rl_simple))

# LOO evaluation ----------------------------------------------------
# log_lik_rl_plus <- extract_log_lik(rl_plus_model, merge_chains = FALSE)

# loo_rl_plus <- loo::loo(log_lik_rl_plus,
# print(loo_rl_plus)
#  r_eff_rl_plus <- relative_eff(exp(log_lik_rl_plus), cores = 4)

# loo_rl_plus <- loo::loo(log_lik_rl_plus,
# 	r_eff = r_eff_rl_plus, cores = 4)
#  saveRDS(loo_rl_plus, file.path(saved_objects_path, 'loo_rl_plus.RDS'))
# print(loo_rl_plus)
