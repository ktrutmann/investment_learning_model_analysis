library(rstan)
library(tidyverse)

theme_set(theme_minimal())

rl_single_model <- readRDS(
	file.path('..', 'data', 'saved_objects',
		'210122_rl_simple_main_study.RDS'))

# Diagnostics -----------------------------------------------

pairs(rl_single_model, pars = names(rl_single_model@sim$samples[[1]])[
	c(1, 2, 195, 196)])

pairs(rl_single_model, pars = names(rl_single_model@sim$samples[[1]])[
	c(389:399)])

sink(file.path('..', 'data', 'saved_objects',
	'210112_rl_single_main_summary.txt'))
summary(rl_single_model)
sink()

# Get Posteriors -------------------------------------------------
model_traces <- rstan::extract(rl_single_model,
	pars = c('hyper_alpha', 'hyper_alpha_sd', 'hyper_sigma', 'hyper_sigma_sd'),
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble() %>%
	mutate(hyper_alpha = pnorm(hyper_alpha))

# Plots: ------------------------------------------------
model_traces %>%
	ggplot(aes(hyper_alpha)) +
	geom_histogram(bins = 200, fill = 'skyblue4')

ggsave('single_hyper_alpha_hist.pdf',  device = 'pdf',
  width = 10, height = 7, path = file.path('output', 'figures'))

model_traces %>%
	ggplot(aes(hyper_alpha_sd)) +
	geom_histogram(bins = 200, fill = 'skyblue4')

# Note: Alphas start at param no. 389
rstan::extract(rl_single_model,
	pars = names(rl_single_model@sim$samples[[1]])[c(430)],
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble() %>%
	pivot_longer(cols = everything()) %>%
	ggplot(aes(x = value)) +
		geom_histogram(fill = 'skyblue4', bins = 100) +
		facet_wrap(facets = vars(name))