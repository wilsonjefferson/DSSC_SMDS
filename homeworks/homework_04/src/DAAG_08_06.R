# LIBRARIES:
library(MASS)       # confint for GLMs
library(lattice)
library(DAAG)


x_grid <- seq(from=0.00, to=1.00, by=0.01)


### POISSON DATA ###


# Generation
poiss_a <- DAAG::poissonsim(x_grid, a = 2, b = -4)

# Fit (Poisson)
poiss_a_fit <- glm(poiss_a$y~poiss_a$x, family = poisson)

# Summary
summary(poiss_a_fit)

# C.I.s
confint(poiss_a_fit)



### OVERDISPERSED POISSON DATA ###


# Generation
poiss_b <- DAAG::poissonsim(x_grid, a = 2, b = -4, slope.sd = 5)

# Fit (Poisson)
poiss_b_fit  <- glm(poiss_b$y~poiss_b$x, family = poisson)

# Summary
summary(poiss_b_fit)

# C.I.s
confint(poiss_b_fit)


# Fit (Quasi-Poisson)
qpoiss_b_fit <- glm(poiss_b$y~poiss_b$x, family = quasipoisson)

# Summary
summary(qpoiss_b_fit)

# C.I.s
confint(qpoiss_b_fit)
