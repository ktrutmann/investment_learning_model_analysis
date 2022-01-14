# Note: As the .csv files containing all sampls are rather large you should use
# read_stan_csv(csvfiles, col_major = TRUE) to load one after the other and extract the
# variable of interest.

# library(loo)
# library(plotly)
# library(shinystan)
# library(bayesplot)
# library(tidybayes)
library(ggdist)
library(rstan)
library(tidyverse)

renv::activate()
theme_set(theme_minimal())

rl_plus_model <- readRDS(
	file.path('..', 'data', 'saved_objects',
		'210204_rl_plus_main_study_50k.RDS'))

# Diagnostics -----------------------------------------------

pairs(rl_plus_model) #, pars = names(rl_plus_model@sim$samples[[1]])[c(1, 2, 6, 7)])


# Get Posteriors -------------------------------------------------
# hyper_alpha_sds to get the standard deviation estimates.
# Also remember to remove the last line when doing so
model_traces <- rstan::extract(rl_plus_model,
	pars = str_c('hyper_alphas[', 1:5, ']'),
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble() %>%
	transmute(across(everything(), pnorm))

# saveRDS(model_traces, file.path('..', 'data', 'saved_objects',
# 		'samples_rl_plus_main_study_50k.RDS'))
model_traces <- readRDS(file.path('..', 'data', 'saved_objects',
	'samples_rl_plus_main_study_50k.RDS'))

model_traces_long <- model_traces %>%
	pivot_longer(cols = everything()) %>%
	transmute(context = factor(name,
		levels = str_c('hyper_alphas[', c(1, 2, 4, 3, 5), ']'),
		labels = c('Not Inv.', 'Fav. Gain', 'Fav. Loss',
			'Unfav. Gain', 'Unfav. Loss')),
		alpha_value = value)

group_by(model_traces_long, context) %>%
	summarize(mean = mean(alpha_value),
		median = median(alpha_value),
		quant_05 =  quantile(alpha_value, .05),
		quant_95 =  quantile(alpha_value, .95)) %>%
	mutate(across(where(is.numeric), round, 2)) %>%
	knitr::kable(format = 'latex')

# Plots: ------------------------------------------------
model_traces %>%
	mutate(alpha_diff = (`hyper_alphas[5]` - `hyper_alphas[4]`) -
		(`hyper_alphas[3]` - `hyper_alphas[2]`),
		ci_95 = quantile(alpha_diff, .95)) %>%
	ggplot(aes(alpha_diff, fill = alpha_diff > ci_95)) +
	geom_histogram(bins = 200) +
	geom_vline(xintercept = 0, size = 1) +
	scale_fill_manual(values = c('skyblue4', 'skyblue2')) +
	labs(x = 'Parameters (Unfavorable Loss - Favorable Loss) - (Unfavorable Gain - Favorable Gain)',
		y = 'Frequency') +
	theme(legend.position = 'none',
		text = element_text(size = 16))

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
# Note: Alphas go from 1166 zo 2125
rstan::extract(rl_plus_model,
	pars = names(rl_plus_model@sim$samples[[1]])[c(1357:1381)],
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble() %>%
	pivot_longer(cols = everything()) %>%
	ggplot(aes(x = value)) +
		geom_histogram(fill = 'skyblue4', bins = 100) +
		facet_wrap(facets = vars(name))
		

# Violins
model_traces_long %>%
	ggplot(aes(x = context, y = alpha_value)) +
		geom_violin(fill = 'grey') +
		stat_summary(fun.data = 'mean_se', geom = 'crossbar', color = 'black') +
		labs(x = 'Context', y = 'Learning Rate') +
		theme(text = element_text(size = 16))

ggsave('parameter_violins.pdf',  device = 'pdf',
  width = 10, height = 7, path = file.path('output', 'figures'))


# Barplot:
group_by(model_traces_long, context) %>%
	summarize(mean = mean(alpha_value),
		median = median(alpha_value),
		quant_05 =  quantile(alpha_value, .05),
		quant_95 =  quantile(alpha_value, .95)) %>%
	ggplot(aes(x = context, y = mean)) +
		geom_bar(stat = 'identity', fill = 'grey') +
		geom_errorbar(aes(ymin = quant_05, ymax = quant_95),
			width = .1) +
		labs(x = 'Context', y = 'Learning Rate') +
		theme(text = element_text(size = 16))

ggsave('parameter_bars.pdf',  device = 'pdf',
  width = 10, height = 7, path = file.path('output', 'figures'))


# Density Plots


model_traces_long %>%
	ggplot(aes(x = alpha_value, y = context)) +
		ggdist::stat_halfeye(.width = .9, interval_size = 6, slab_alpha = .75,
			# fill = '#A4D6D1') +
			fill = 'darkgrey') +
		labs(x = 'Learning Rate', y = 'Density per Context') +
		scale_y_discrete(labels =
c('Not\nInvested', 'Favorable\nGain', 'Favorable\nLoss', 'Unfavorable\nGain', 'Unfavorable\nLoss')) +
		theme(text = element_text(size = 16),
			axis.text.y = element_text(vjust = -1, hjust = .5))

ggsave('parameter_densities.pdf',  device = 'pdf',
  width = 10, height = 7, path = file.path('output', 'figures'))