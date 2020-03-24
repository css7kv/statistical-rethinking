# Grid approximation (pg 40)
# Globe example

# define number of points on grid
n <- 5

# define grid - possible probabilities of W
p_grid <- seq(from=0, to=1, length.out=n)
p_grid

# define prior 
#prior <- rep(1, n) # equally likely for all values of p
#prior <- ifelse(p_grid < 0.5, 0, 1) # impossible for <0.5, else equally likely
prior <- exp(-5*abs(p_grid - 0.5))
prior

# compute likelihood at each value in grid
# the plausibility the data (6 Ws in 9 tosses) can be seen for each probability of W
# the d in dbinom means it will output the density 
likelihood <- dbinom(6, size=9, prob=p_grid)
likelihood

# compute unstandardized posterior
unstd.posterior <- likelihood*prior
unstd.posterior

# compute standardized posterior (sums to 1)
posterior <- unstd.posterior/sum(unstd.posterior)
posterior

# display prior distribution
plot(p_grid, prior, type="b",
     xlab="probability of water", ylab="posterior probability")

# display posterior distribution
plot(p_grid, posterior, type="b",
     xlab="probability of water", ylab="posterior probability")

