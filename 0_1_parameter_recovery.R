library(rstan)
library(tidyverse)
library(GGally)
library(ggdist)
library(patchwork)

theme_set(theme_minimal())

# Get the original data to get the true parameters
dat_long <- read_delim(file.path('..', 'data',
	'clean', 'all_participants_long_main_param_recov.csv'),
	delim = ';')

# Get the samples from the model fitting:
fitted_model <- readRDS(
	file.path('..', 'data', 'saved_objects',
		'220713-1500_param_recov.RDS'))

# Hyperparameters
hyper_alphas <- rstan::extract(fitted_model, 'hyper_alphas')[[1]]
dimnames(hyper_alphas)[[2]] <- str_c('alpha_',
	c('not_inv', 'fav_gain', 'unfav_gain', 'fav_loss', 'unfav_loss'))
hyper_alphas <- as_tibble(hyper_alphas) %>%
	mutate(across(.fns = pnorm))
	
# Individual parameters
individual_alphas <- rstan::extract(fitted_model, 'alphas') %>%
	pluck(1) %>%
	apply(c(2:3), median)
dimnames(individual_alphas)[[2]] <- str_c('alpha_',
	c('not_inv', 'fav_gain', 'unfav_gain', 'fav_loss', 'unfav_loss'))
individual_alphas <- as_tibble(individual_alphas) %>%
	add_column(participant_code = unique(dat_long$participant_code))

# Building a dataset:
full_dat <- dat_long %>%
	filter(round_number == 1) %>%
	select(participant_code, alpha_shift) %>%
	mutate(alpha_unaltered = .2 + alpha_shift,
		alpha_altered = .15 + alpha_shift) %>%
	full_join(individual_alphas, by = 'participant_code')


# Individual Recoveries ----
# Not inv:
cor.test(full_dat$alpha_unaltered, full_dat$alpha_not_inv)
this_cor <- cor(full_dat$alpha_unaltered, full_dat$alpha_not_inv) %>%
	round(3) %>%
	str_replace('0.', '.')

scatter_not_inv <- ggplot(full_dat,
	aes(alpha_unaltered, alpha_not_inv)) +
	geom_point() +
	geom_abline(slope = 1, intercept = 0) +
	annotate('text', x = .16, y = .215, label = str_c('r =', this_cor), size = 6) +
	labs(title = 'Not Invested', x = 'True Value', y = 'Recovered Value')

# fav gain:
cor.test(full_dat$alpha_altered, full_dat$alpha_fav_gain)
this_cor <- cor(full_dat$alpha_altered, full_dat$alpha_fav_gain) %>%
	round(3) %>%
	str_replace('0.', '.')

scatter_fav_gain <- ggplot(full_dat,
	aes(alpha_altered, alpha_fav_gain)) +
	geom_point() +
	geom_abline(slope = 1, intercept = 0) +
	annotate('text', x = .11, y = .16, label = str_c('r =', this_cor), size = 6) +
	labs(title = 'Gain Favorable', x = 'True Value', y = 'Recovered Value')

# fav loss:
cor.test(full_dat$alpha_unaltered, full_dat$alpha_fav_loss)
this_cor <- cor(full_dat$alpha_unaltered, full_dat$alpha_fav_loss) %>%
	round(3) %>%
	str_replace('0.', '.')

scatter_fav_loss <- ggplot(full_dat,
	aes(alpha_unaltered, alpha_fav_loss)) +
	geom_point() +
	geom_abline(slope = 1, intercept = 0) +
	annotate('text', x = .16, y = .212, label = str_c('r =', this_cor), size = 6) +
	labs(title = 'Loss Favorable', x = 'True Value', y = 'Recovered Value')

# unfav gain:
cor.test(full_dat$alpha_unaltered, full_dat$alpha_unfav_gain)
this_cor <- cor(full_dat$alpha_unaltered, full_dat$alpha_unfav_gain) %>%
	round(3) %>%
	str_replace('0.', '.')

scatter_unfav_gain <- ggplot(full_dat,
	aes(alpha_unaltered, alpha_unfav_gain)) +
	geom_point() +
	geom_abline(slope = 1, intercept = 0) +
	annotate('text', x = .16, y = .21, label = str_c('r =', this_cor), size = 6) +
	labs(title = 'Gain Unfavorable', x = 'True Value', y = 'Recovered Value')

# unfav loss:
cor.test(full_dat$alpha_altered, full_dat$alpha_unfav_loss)
this_cor <- cor(full_dat$alpha_altered, full_dat$alpha_unfav_loss) %>%
	round(3) %>%
	str_replace('0.', '.')

scatter_unfav_loss <- ggplot(full_dat,
	aes(alpha_altered, alpha_unfav_loss)) +
	geom_point() +
	geom_abline(slope = 1, intercept = 0) +
	annotate('text', x = .11, y = .18, label = str_c('r =', this_cor), size = 6) +
	labs(title = 'Loss Unfavorable', x = 'True Value', y = 'Recovered Value')

scatter_not_inv + scatter_fav_gain + scatter_unfav_gain +
scatter_fav_loss + scatter_unfav_loss +
plot_layout(design = "AABBCC
					  #DDEE#")

ggsave(file.path('..', 'output', 'param_recov_individual_scatter.pdf'),
	width = 25, height = 15, units = 'cm')


# Distributions of individual estimates:
full_dat %>%
	pivot_longer(cols = matches('not|fav|gain|loss'),
	names_to = 'param', values_to = 'alpha_value') %>%
	ggplot(aes(alpha_value, fill = param)) +
		geom_density(alpha = .5)


# Hyperparameters: ---------------------
hyper_alphas %>%
	pivot_longer(everything()) %>%
	group_by(name) %>%
	summarise(mean = mean(value),
		median = median(value),
		sd = sd(value),
		ci90_lower = quantile(value, .05),
		ci90_upper = quantile(value, .95))

# Hyper-params posterior:
hyper_alphas %>%
	pivot_longer(everything()) %>%
	ggplot(aes(value, fill = name)) +
		geom_density(alpha = .5) +
		geom_vline(xintercept = .2) +
		geom_vline(xintercept = .15)

# Separate hyper-parameters density plots:
hyper_alphas %>%
	pivot_longer(everything()) %>%
	mutate(true_val = if_else(
		name %in% c('alpha_fav_gain', 'alpha_unfav_loss'), .15, .2)) %>%
	ggplot(aes(value)) +
		facet_wrap('name', scale = 'free_x', switch = 'x',
			labeller = as_labeller(c(
				'alpha_not_inv' = 'Not Invested',
				'alpha_unfav_loss' = 'Loss Unfavorable',
				'alpha_fav_loss' = 'Loss Favorable',
				'alpha_unfav_gain' = 'Gain Unfavorable',
				'alpha_fav_gain' = 'Gain Favorable'))) +
		stat_halfeye() +
		geom_vline(aes(xintercept = true_val)) +
		labs(x = 'Recovered Learning Rates', y = 'Posterior Probability Density') +
		theme(panel.spacing.x = unit(1, 'lines'))

ggsave(file.path('..', 'output', 'param_recov_hyper_alpha_dens.pdf'),
	width = 20, height = 12, units = 'cm')


ggpairs(hyper_alphas) # Checking out the correlations

# Interaction:
hyper_alphas %>%
	mutate(interaction_samples =
		(alpha_fav_gain - alpha_fav_loss) - (alpha_unfav_gain - alpha_unfav_gain)) %>%
	ggplot(aes(interaction_samples,
		fill = interaction_samples < quantile(interaction_samples, .95))) +
		geom_histogram()