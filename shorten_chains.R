# This function tries to retroactively filter out the warmup samples which were
# erroniously saved due to this bug: https://github.com/stan-dev/rstan/issues/649
# The samples are not really saved, but each chain gets a trailing sequence of zeroes with the length of the warmup samples.
# The goal is to avoid this error when bridge sampling:
# https://github.com/quentingronau/bridgesampling/issues/21


shorten_chains <- function(rstan_obj){
	# Determine true number of samples
	n_real_samples <- rstan_obj@stan_args[[1]]$iter -
		rstan_obj@stan_args[[1]]$warmup[1]

	for (i_chain in 1:rstan_obj@sim$chains){
		rstan_obj@sim$samples[[i_chain]] <-
			rstan_obj@sim$samples[[i_chain]][1:n_real_samples, ]
	
		attr(rstan_obj@sim$samples[[i_chain]], 'row.names') <-
			attr(rstan_obj@sim$samples[[i_chain]], 'row.names')[
				1:n_real_samples]

		attr(rstan_obj@sim$samples[[i_chain]], 'sampler_params') <-
			attr(rstan_obj@sim$samples[[i_chain]], 'sampler_params')[1:n_real_samples, ]

		rstan_obj@sim$n_save[i_chain] <- n_real_samples
	}
return(rstan_obj)
}
