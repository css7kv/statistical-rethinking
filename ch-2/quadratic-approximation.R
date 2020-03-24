library(rethinking)

globe.qa <- rethinking::map(
    alist(
        w ~ dbinom(9, p), # binomial likelihood
        p ~ dunif(0, 1) # uniform prior
    ),
    data=list(w=6))
globe.qa

# display summary
rethinking::precis(globe.qa)

# analytical calcuation (known posterior)
w <- 6
n <- 9
curve(dbeta(x, w+1, n-w+1), from=0, to=1)

# quadratic approximation
curve(dnorm(x, 0.67, .16), lty=2, add=TRUE)
