library(rstan)
library(tidyverse)

theme_set(theme_minimal())

# Get the original data to get the true parameters
dat_long <- read_delim(file.path('..', 'data',
	'clean', 'all_participants_long_main_param_recov.csv'),
	delim = ';')

# Get the samples from the model fitting:
my_samples <- readRDS(
	file.path('..', 'data', 'saved_objects', 'param_recov',
		'220617_rl_plus_param_recov_alpha_samples.RDS'))

# Hyperparameters
attr(my_samples$hyper_alphas, 'dimnames') <- list(
	iterations = NULL,
	parameters = c('not_inv', 'fav_gain', 'unfav_gain', 'fav_loss', 'unfav_loss'))
hyper_alphas <- as_tibble(my_samples$hyper_alphas) %>%
	mutate(across(.fns = pnorm))
	
# Individual parameters
individual_alphas <- apply(my_samples$alphas, c(2:3), median)
dimnames(individual_alphas)[[2]] <- str_c('alpha_',
	c('not_inv', 'fav_gain', 'unfav_gain', 'fav_loss', 'unfav_loss'))
individual_alphas <- as_tibble(individual_alphas) %>%
	mutate(across(.fns = pnorm)) # TODO: (1) Reconstruct alpha! These are "raw"!
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

# fav gain:
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


# Find out what's wrong with fav loss...
dat_long <- dat_long %>%
	mutate(updated_from = paste(position_end_of_last_period,
		price_move_from_last_corrected),
		updated_from = case_when(
			str_detect(updated_from, 'NA') ~ NA_character_,
			str_detect(updated_from, 'No') ~ 'Not Invested',
			TRUE ~ updated_from))

count(dat_long, updated_from) # It's not a lack of cases...