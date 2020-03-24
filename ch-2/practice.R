#### 2.6 - Practice
library(tidyverse)

# 2M1 - grid approximation
length_grid <- 20
p_grid <- seq(0, 1, length.out=length_grid)
prior <- rep(1, length_grid)
p_grid_df <- data.frame(p_grid)

# W, W, W
n <- 3
w <- 3
likelihood <- dbinom(w, n, p_grid)
unstd.posterior <- likelihood*prior
p_grid_df$posterior <- unstd.posterior/sum(unstd.posterior)
p_grid_df %>%
    ggplot(aes(p_grid, posterior)) +
    geom_point() +
    geom_line()

# W, W, W, L
n <- 4
w <- 3
likelihood <- dbinom(w, n, p_grid)
unstd.posterior <- likelihood*prior
p_grid_df$posterior <- unstd.posterior/sum(unstd.posterior)
p_grid_df %>%
    ggplot(aes(p_grid, posterior)) +
    geom_point() +
    geom_line()

# L, W, W, L, W, W, W
n <- 7
w <- 5
likelihood <- dbinom(w, n, p_grid)
unstd.posterior <- likelihood*prior
p_grid_df$posterior <- unstd.posterior/sum(unstd.posterior)
p_grid_df %>%
    ggplot(aes(p_grid, posterior)) +
    geom_point() +
    geom_line()

# 2M2 - Change the prior
length_grid <- 20
p_grid <- seq(0, 1, length.out=length_grid)
prior <- ifelse(p_grid < 0.5, 0, 1)
p_grid_df <- data.frame(p_grid)

# W, W, W
n <- 3
w <- 3
likelihood <- dbinom(w, n, p_grid)
unstd.posterior <- likelihood*prior
p_grid_df$posterior <- unstd.posterior/sum(unstd.posterior)
p_grid_df %>%
    ggplot(aes(p_grid, posterior)) +
    geom_point() +
    geom_line()

# W, W, W, L
n <- 4
w <- 3
likelihood <- dbinom(w, n, p_grid)
unstd.posterior <- likelihood*prior
p_grid_df$posterior <- unstd.posterior/sum(unstd.posterior)
p_grid_df %>%
    ggplot(aes(p_grid, posterior)) +
    geom_point() +
    geom_line()

# L, W, W, L, W, W, W
n <- 7
w <- 5
likelihood <- dbinom(w, n, p_grid)
unstd.posterior <- likelihood*prior
p_grid_df$posterior <- unstd.posterior/sum(unstd.posterior)
p_grid_df %>%
    ggplot(aes(p_grid, posterior)) +
    geom_point() +
    geom_line()

# 2M3 
data.frame(outcomes=c("land", "earth")) %>%
    mutate(prior_proba = .5, # globes are equally likely
           likelihood_w = c(.3, 1), #70% of Earth is water, 100% of Mars is land
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior)) %>%
    filter(outcomes == "land") %>%
    pull(posterior)

# 2M4
data.frame(outcomes=c("two_black_sides", "two_white_sides", "one_black_one_white")) %>%
    mutate(prior_proba = 1/3, # each of the 3 cards is equally likely
           likelihood_w = c(1, 0, .5), # probability of seeing the single black side
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior)) %>%
    filter(outcomes == "two_black_sides") %>%
    pull(posterior)

# 2M5
data.frame(outcomes=c("two_black_sides", "two_white_sides", "one_black_one_white")) %>%
    mutate(prior_proba = c(.5, .25, .25), # in this instance there are 2 black sided cards, and one of all other types
           likelihood_w = c(1, 0, .5), # probability of seeing the single black side
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior)) %>%
    filter(outcomes == "two_black_sides") %>%
    pull(posterior)

# 2M6
data.frame(outcomes=c("two_black_sides", "two_white_sides", "one_black_one_white")) %>%
    mutate(prior_proba = c(1, 3, 2), # probability of drawing different cards has changed
           likelihood_w = c(1, 0, .5), # probability of seeing the single black side
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior)) %>%
    filter(outcomes == "two_black_sides") %>%
    pull(posterior)

# 2M7
data.frame(outcomes=c("two_black_sides", "two_white_sides", "one_black_one_white")) %>%
    mutate(prior_proba = 1, # equal likelihood of drawing 3 cards, uniform constant
           likelihood_w = c(2, 0, 1), # 2 ways it can be shown for B/B, 0 ways for W/W, 1 way for B/W
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior),
           likelihood_w2 = c(3, 1, 2), # 3 ways it can be shown for B/B, 1 way for W/W, 2 ways for B/W
           unstd.posterior2 = posterior*likelihood_w2,
           posterior2 = unstd.posterior2/sum(unstd.posterior2)) %>%
    filter(outcomes == "two_black_sides") %>% 
    pull(posterior2)

# 2H1
df <- data.frame(outcomes=c("speciesA", "speciesB")) %>%
    mutate(prior_proba = 1, # equal likelihood of being a species
           likelihood_w = c(.1, .2), # first observation: birthed twins. each has different proba of birthing twins
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior))
# posterior probability = probability of being a certain species
# so, probability of next birth being a twin is Pr(species_a)*Pr(birth_twins|species_a) + Pr(species_b)*Pr(birth_twins|species_b)
df <- df %>%
    mutate(prob_birth_twins = c(.1, .2),
           prob_second_birth_is_twins = prob_birth_twins*posterior)
sum(df$prob_second_birth_is_twins) # .167

# 2H2
data.frame(outcomes=c("speciesA", "speciesB")) %>%
    mutate(prior_proba = 1, # equal likelihood of being a species
           likelihood_w = c(.1, .2), # first observation: birthed twins. each has different proba of birthing twins
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior)) %>%
    filter(outcomes == "speciesA") %>%
    pull(posterior)

# 2H3
data.frame(outcomes=c("speciesA", "speciesB")) %>%
    mutate(prior_proba = 1, # equal likelihood of being a species
           likelihood_w = c(.1, .2), # first observation: birthed twins. each has different proba of birthing twins
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior)) %>%
    mutate(prior_proba = posterior,
           likelihood_w = c(.9, .8), # second observation: birthed single infant
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior)
           ) %>%
    filter(outcomes == "speciesA") %>%
    pull(posterior)

# 2H4
# Pr(test_a, a) = Pr(test_a|a)*Pr(a) = .8*.5 
# Pr(test_a, b) = Pr(test_a|b)*Pr(b) = (1-.65)*.5
obs1 <- data.frame(outcomes=c("speciesA", "speciesB")) %>%
    mutate(prior_proba = 1,
           likelihood_w = c(.8*.5, (1-.65)*.5), # first observation: vet test. probabilities according to comments above
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior))
obs1 %>% filter(outcomes == 'speciesA') %>% pull(posterior)

obs2 <- obs1 %>%
    mutate(prior_proba = posterior,
           likelihood_w = c(.1, .2), # second observation: birthed twins, each has a different proba of birthing twins
           unstd.posterior = prior_proba*likelihood_w,
           posterior = unstd.posterior/sum(unstd.posterior))
obs2 %>% filter(outcomes == 'speciesA') %>% pull(posterior)






