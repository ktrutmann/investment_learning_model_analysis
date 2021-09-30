library(rstan)
library(tidyverse)

theme_set(theme_minimal())

rl_single_model <- readRDS(
	file.path('..', 'data', 'saved_objects',
		'210205_rl_simple_main_study_50k.RDS'))

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

	model_traces %>%
		pivot_longer(everything()) %>%
		group_by(name) %>%
		summarize(mean = mean(value),
			qt5 = quantile(value, .05),
			qt95 = quantile(value, .95))


	summarize(across(everything(), list(mean, sd)))

# Plots: ------------------------------------------------
model_traces %>%
	ggplot(aes(hyper_alpha)) +
	geom_histogram(bins = 200, fill = 'skyblue4')

summary(model_traces$hyper_alpha)
quantile(model_traces$hyper_alpha, c(.05, .95))


ggsave('single_hyper_alpha_hist.pdf',  device = 'pdf',
  width = 10, height = 7, path = file.path('output', 'figures'))

model_traces %>%
	ggplot(aes(hyper_alpha_sd)) +
	geom_histogram(bins = 200, fill = 'skyblue4')

# Note: Alphas start at param no. 389
rstan::extract(rl_single_model,
	pars = names(rl_single_model@sim$samples[[1]])[389:397 + 18],
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble() %>%
	pivot_longer(cols = everything()) %>%
	ggplot(aes(x = value)) +
		geom_histogram(fill = 'skyblue4', bins = 100) +
		facet_wrap(facets = vars(name))