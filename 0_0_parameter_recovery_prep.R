# This was run on the SciCORE cluster as it handles some quite large data.

# For more about this function see here:
# https://gist.github.com/ktrutmann/e8c5b47f8467ac7f60e56412ab899cdd
shorten_chains <- function(rstan_obj) {
	# Determine true number of samples
	n_real_samples <- rstan_obj@stan_args[[1]]$iter -
		rstan_obj@stan_args[[1]]$warmup[1]

	for (i_chain in 1:rstan_obj@sim$chains) {
		rstan_obj@sim$samples[[i_chain]] <-
			rstan_obj@sim$samples[[i_chain]][1:n_real_samples, ]

		attr(rstan_obj@sim$samples[[i_chain]], 'row.names') <-
			attr(rstan_obj@sim$samples[[i_chain]], 'row.names')[
				1:n_real_samples]

		attr(rstan_obj@sim$samples[[i_chain]], 'sampler_params') <-
			attr(rstan_obj@sim$samples[[i_chain]], 'sampler_params')[1:n_real_samples, ]

		rstan_obj@sim$permutation[[i_chain]] <-
			rstan_obj@sim$permutation[[i_chain]][which(
			rstan_obj@sim$permutation[[i_chain]] <= n_real_samples)]

		rstan_obj@sim$n_save[i_chain] <- n_real_samples
	}
return(rstan_obj)
}

# Get the samples from the model fitting:
fit_dat <- readRDS(
	file.path('..', 'data', 'saved_objects', 'param_recov',
		'220610_rl_plus_param_recov.RDS'))

# Make it more managable. Right now we're only interested in alphas:
for (i_chain in 1:4) {
	fit_dat@sim$samples[[i_chain]] <-
	fit_dat@sim$samples[[i_chain]][,
		str_detect(names(fit_dat@sim$samples[[i_chain]]), 'alphas\\.')]

	attr(fit_dat@sim$samples[[i_chain]], 'sampler_params') <-
		attr(fit_dat@sim$samples[[i_chain]], 'sampler_params')[
		, str_detect(names(fit_dat@sim$samples[[i_chain]]), 'alpha.')]
}

fit_dat@sim$fnames_oi <- str_subset(fit_dat@sim$fnames_oi, 'alphas\\[')
fit_dat@sim$pars_oi <- c('hyper_alphas', 'alphas')
fit_dat@sim$dims_oi <- fit_dat@sim$dims_oi[
	names(fit_dat@sim$dims_oi) %in% fit_dat@sim$pars_oi]

fit_dat <- shorten_chains(fit_dat)

saveRDS(fit_dat, file.path('data', 'saved_objects',
	'220609_cut_down_param_recov.rds'))