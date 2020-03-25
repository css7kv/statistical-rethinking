# computing the posterior for globe example
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1,1000)
likelihood <- dbinom(6, size=9, prob=p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)

#### Intervals of defined boundaries
# Ex: what is the probability that the proportion of water is less than .5?
sum(posterior[p_grid < 0.5]) # Method 1: using posterior (easy when only one parameter)
samples <- sample(p_grid, prob=posterior, size=10000, replace=TRUE)
sum(samples < 0.5)/length(samples) # Method 2: using samples (required when more complex)

#### Intervals of defined mass
# Ex: where does the lower 80th percentile lie
quantile(samples, .8)
# Ex: where does the middle 80th percentile lie
quantile(samples, c(.1, .9))
# Quantiles work when distribution is relatively symmetrical
# Asymmetrical example:
likelihood <- dbinom(3, size=3, prob=p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, prob=posterior, size=10000, replace=TRUE)
plot(samples)
rethinking::dens(samples)
# using rethinking methods to compute in asymmetrical cases
PI(samples, prob=.5) # percentile interval (in the middle)
HPDI(samples, prob=.5) # highest posterior density interval

#### Point estimates
# Max posterior probability
p_grid[which.max(posterior)] # using posterior
rethinking::chainmode(samples, adj=.01) # using samples
# Posterior mean? median?
mean(samples)
median(samples)
# Using loss functions -- let's say loss is proportional to distance from true value
sum(posterior*abs(.5-p_grid)) # expected loss if we choose point estimate = .5
loss <- sapply(p_grid, function(d) sum(posterior*abs(d-p_grid))) # expected loss for each point estimate
p_grid[which.min(loss)] # that p_grid value which minimizes loss => turns out to be the median
