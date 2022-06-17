library(rstan)
library(tidyverse)

theme_set(theme_minimal())

# Get the original data to get the true parameters
dat_long <- read_delim(file.path('..', 'Data',
	'Clean', 'all_participants_long_main_param_recov.csv'),
	delim = ';')

# Get the samples from the model fitting:
fit_dat <- readRDS(
	file.path('..', 'Data', 'saved_objects', 'param_recov',
		'220609_cut_down_param_recov.rds'))

my_samples <- rstan::extract(fit_dat)

individual_alphas <- apply(my_samples$alphas, c(2:3), median)
dimnames(individual_alphas)[[2]] <- str_c('alpha_',
	c('not_inv', 'fav_gain', 'unfav_gain', 'fav_loss', 'unfav_loss'))
individual_alphas <- as_tibble(individual_alphas)
individual_alphas$participant_code <- unique(dat_long$participant_code)

# Building a dataset:
full_dat <- dat_long %>%
	filter(round_number == 1) %>%
	select(participant_code, alpha_shift) %>%
	mutate(alpha_unaltered = .2 + alpha_shift,
		alpha_altered = .12 + alpha_shift) %>%
	full_join(individual_alphas, by = 'participant_code')

# Not inv:
cor.test(full_dat$alpha_unaltered, full_dat$alpha_not_inv)

ggplot(full_dat,
	aes(alpha_unaltered, alpha_not_inv)) +
	geom_point() +
	geom_abline(slope = 1, intercept = 0)

# fav gain: WAT?
cor.test(full_dat$alpha_altered, full_dat$alpha_fav_gain)

ggplot(full_dat,
	aes(alpha_altered, alpha_fav_gain)) +
	geom_point() +
	geom_abline(slope = 1, intercept = 0)

# fav loss: WAT?
cor.test(full_dat$alpha_unaltered, full_dat$alpha_fav_loss)

ggplot(full_dat,
	aes(alpha_unaltered, alpha_fav_loss)) +
	geom_point() +
	geom_abline(slope = 1, intercept = 0)

# unfav gain:
cor.test(full_dat$alpha_unaltered, full_dat$alpha_unfav_gain)

ggplot(full_dat,
	aes(alpha_unaltered, alpha_unfav_gain)) +
	geom_point() +
	geom_abline(slope = 1, intercept = 0)

# unfav loss:
cor.test(full_dat$alpha_unaltered, full_dat$alpha_unfav_loss)

ggplot(full_dat,
	aes(alpha_unaltered, alpha_unfav_loss)) +
	geom_point() +
	geom_abline(slope = 1, intercept = 0)


# Hyperparameters:
hyper_alphas <- as_tibble(my_samples$hyper_alpha) %>%
	rename(not_inv = V1, fav_gain = V2, unfav_gain = V3,
		fav_loss = V4, unfav_loss = V5) %>%
	mutate(across(.fns = pnorm))
	
ggplot(pivot_longer(hyper_alphas, everything()),
	aes(value, fill = name)) +
	geom_density(alpha = .5)

hyper_alphas %>%
	pivot_longer(everything()) %>%
	group_by(name) %>%
	summarise(mean = mean(value),
		sd = sd(value))


# Distributions:
full_dat %>%
	pivot_longer(cols = matches('not|fav|gain|loss'),
	names_to = 'param', values_to = 'alpha_value') %>%
	ggplot(aes(alpha_value, fill = param)) +
		geom_density(alpha = .6)
