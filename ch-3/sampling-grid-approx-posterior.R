# computing the posterior for globe example
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1,1000)
likelihood <- dbinom(6, size=9, prob=p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)

# 10,000 samples
samples <- sample(p_grid, prob=posterior, size=10000, replace=TRUE)

# birds eye view of samples
plot(samples)

# density estimate from samples
rethinking::dens(samples)
