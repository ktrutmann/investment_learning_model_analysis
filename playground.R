library(rstan)
library(tidyverse)
library(bridgesampling)


derp <- tibble(x = rnorm(1000),
	group = rep(0:1, each = 500)) %>%
	mutate(x = case_when(
		group == 0 ~ x + 5,
		group == 1 ~ x - 5))

ggplot(derp,
	aes(x = x, fill = as.factor(group))) +
	geom_histogram(bins = 50)

derp <- as.list(derp)
derp$n <- length(derp[[1]])

# stan models ----------------------
# The model with only one parameter
h0_model <- '
data{
	int n;
	real x[n];
}

parameters{
	real mu;
	real<lower = 0> sigma;
}

model{
	mu ~ normal(0, 3);
	sigma ~ lognormal(.8, 1);

	for (i in 1:n){
		target += normal_lpdf(x[i] | mu, sigma);
	}
}
'

# The model with two parameters but the same priors
h1_model <- '
data{
	int n;
	real x[n];
	int group[n];
}

parameters{
	real mu1;
	real mu2;
	real<lower = 0> sigma;
}

model{
	mu1 ~ normal(0, 3);
	mu2 ~ normal(0, 3);
	sigma ~ lognormal(.8, 1);

	for (i in 1:n){
		if (group[i] == 1){
			target += normal_lpdf(x[i] | mu1, sigma);
		} else {
			target += normal_lpdf(x[i] | mu2, sigma);
		}
	}
}
'

# The model with priors that actually represent the expected difference
h1_2_model <- '
data{
	int n;
	real x[n];
	int group[n];
}

parameters{
	real mu1;
	real mu2;
	real<lower = 0> sigma;
}

model{
	mu1 ~ normal(-5, 1);
	mu2 ~ normal(5, 1);
	sigma ~ lognormal(.8, 1);

	for (i in 1:n){
		if (group[i] == 1){
			target += normal_lpdf(x[i] | mu1, sigma);
		} else {
			target += normal_lpdf(x[i] | mu2, sigma);
		}
	}
}
'

h0_fit <- stan(data = derp, model_code = h0_model, chains = 4, cores = 4)
h1_fit <- stan(data = derp, model_code = h1_model, chains = 4, cores = 4)
h1_2_fit <- stan(data = derp, model_code = h1_2_model, chains = 4, cores = 4)

marginal_lik_h0 <- bridge_sampler(h0_fit)
marginal_lik_h1 <- bridge_sampler(h1_fit)
marginal_lik_h1_2 <- bridge_sampler(h1_2_fit)

(this_bf <- bridgesampling::bf(marginal_lik_h0, marginal_lik_h1))

trace_h0 <- rstan::extract(h0_fit,
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble()
trace_h1 <- rstan::extract(h1_fit,
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble()
trace_h1_2 <- rstan::extract(h1_2_fit,
	permuted = TRUE, inc_warmup = FALSE) %>%
	as_tibble()

qplot(trace_h0$sigma)