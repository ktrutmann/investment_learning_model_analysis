# Note: As the .csv files containing all sampls are rather large you should use
# read_stan_csv(csvfiles, col_major = TRUE) to load one after the other and extract the
# variable of interest.

# library(loo)
# library(plotly)
# library(shinystan)
# library(bayesplot)
# library(tidybayes)
library(rstan)
library(tidyverse)

renv::activate() 
theme_set(theme_minimal())

rl_plus_model <- readRDS(
	file.path('..', 'data', 'saved_objects',
		'210118_rl_plus_param_recov.RDS'))

# Diagnostics -----------------------------------------------

pairs(rl_plus_model) #, pars = names(rl_plus_model@sim$samples[[1]])[c(1, 2, 6, 7)])


# Get Posteriors -------------------------------------------------
model_traces <- rstan::extract(rl_plus_model,
	pars = str_c('hyper_alpha_sds[', 1:5, ']'),
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble() %>%
	transmute(across(everything(), pnorm))

saveRDS(model_traces, file.path('..', 'data', 'saved_objects',
		'samples_rl_plus_main_study_small.RDS'))

# TODO: (1) Check for main effects
model_traces_long <- model_traces %>%
	pivot_longer(cols = everything()) %>%
	transmute(context = factor(name,
		levels = str_c('hyper_alpha_sds[', c(1, 2, 4, 3, 5), ']'),
		labels = c('Not Inv.', 'Fav. Gain', 'Fav. Loss',
			'Unfav. Gain', 'Unfav. Loss')),
		alpha_value = value)

# Plots: ------------------------------------------------
model_traces %>%
	mutate(alpha_diff = (`hyper_alphas[5]` - `hyper_alphas[4]`) -
		(`hyper_alphas[3]` - `hyper_alphas[2]`),
		ci_95 = quantile(alpha_diff, .95)) %>%
	ggplot(aes(alpha_diff, fill = alpha_diff > ci_95)) +
	geom_histogram(bins = 200) +
	geom_vline(xintercept = 0, size = 1) +
	scale_fill_manual(values = c('skyblue4', 'skyblue2')) +
	theme(legend.position = 'none')

ggsave('parameter_interaction_test.pdf',  device = 'pdf',
  width = 10, height = 7, path = file.path('output', 'figures'))


model_traces %>%
	mutate(alpha_diff = (`hyper_alphas[2]` - `hyper_alphas[4]`),
		ci_95 = quantile(alpha_diff, .975)) %>%
	ggplot(aes(alpha_diff, fill = alpha_diff > ci_95)) +
	geom_histogram(bins = 200) +
	geom_vline(xintercept = 0, size = 1) +
	scale_fill_manual(values = c('skyblue4', 'skyblue2')) +
	theme(legend.position = 'none')


# Single parameter histograms:
rstan::extract(rl_plus_model,
	pars = names(rl_plus_model@sim$samples[[1]])[c(2000:2010)],
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble() %>%
	pivot_longer(cols = everything()) %>%
	ggplot(aes(x = value)) +
		geom_histogram(fill = 'skyblue4', bins = 100) +
		facet_wrap(facets = vars(name))

# Violins
model_traces_long %>%
	ggplot(aes(x = context, y = alpha_value)) +
	geom_violin(fill = 'skyblue4') +
	stat_summary(fun.data = 'mean_se', geom = 'crossbar', color = 'darkred') +
	labs(x = 'Context', y = 'Learning Rate') +
	theme(text = element_text(size = 16))

ggsave('parameter_violins.pdf',  device = 'pdf',
  width = 10, height = 7, path = file.path('output', 'figures'))