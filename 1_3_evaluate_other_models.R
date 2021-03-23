library(rstan)
library(tidyverse)

theme_set(theme_minimal())

this_model <- readRDS(
	file.path('..', 'saved_objects',
		# '210311_rl_invested_main_study.RDS'))
		# '210312_rl_returns_main_study.RDS'))
		'210320_rl_moves_main_study.RDS'))

# Diagnostics -----------------------------------------------

pairs(this_model, pars = names(this_model@sim$samples[[1]])[
	1:5])


# Get Posteriors -------------------------------------------------
model_traces <- rstan::extract(this_model,
	pars = str_c('hyper_alphas[', 1:3, ']'),
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble() %>%
	transmute(across(everything(), pnorm))

summary(model_traces)

# Plots: ------------------------------------------------
model_traces %>%
	mutate(alpha_diff = (`hyper_alphas[3]` - `hyper_alphas[2]`),
		ci_95 = quantile(alpha_diff, .95)) %>%
	ggplot(aes(alpha_diff, fill = alpha_diff > ci_95)) +
	geom_histogram(bins = 200) +
	geom_vline(xintercept = 0, size = 1) +
	scale_fill_manual(values = c('skyblue4', 'skyblue2')) +
	theme(legend.position = 'none')

# ggsave('single_hyper_alpha_hist.pdf',  device = 'pdf',
#   width = 10, height = 7, path = file.path('output', 'figures'))

model_traces %>%
	pivot_longer(cols = everything()) %>%
	ggplot(aes(x = value)) +
	geom_histogram(bins = 200) +
	facet_wrap(facets = vars(name))