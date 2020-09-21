library(lattice);
library(DAAG)

# What happens if x2 is generated according to x2 <- rbinom(10, 1, x1)?
# x2 <- rbinom(10, 1, .5)?

set.seed(42)

x1 <- runif(10)

# predictor which will be missing
x2 <- rbinom(10, 1, 1-x1) # observed predictor which depends

# on missing predictor
y <- 5*x1 + x2 + rnorm(10,sd=.1)

# simulated model; coef
# of x2 is positive
y.lm <- lm(y ~ factor(x2)) # model fitted to observed data
coef(y.lm)
plot(factor(x2),y)

# effect of missing variable:
# coefficient of x2 has wrong sign
y.lm2 <- lm(y ~ x1 + factor(x2))

# correct model
coef(y.lm2)


# 1
x2_1 <- rbinom(10, 1, x1)
y_1 <- 5*x1 + x2_1 + rnorm(10,sd=.1)
y_1.lm <- lm(y_1 ~ factor(x2_1)) # model fitted to observed data
coef(y_1.lm)
y_1.lm2 <- lm(y_1 ~ x1 + factor(x2_1))
coef(y_1.lm2)

# 2
x2_2 <- rbinom(10, 1, .5)
y_2 <- 5*x1 + x2_2 + rnorm(10,sd=.1)
y_2.lm <- lm(y_2 ~ factor(x2_2)) # model fitted to observed data
coef(y_2.lm)
y_2.lm2 <- lm(y_2~ x1 + factor(x2_2))
coef(y_2.lm2)
plot(factor(x2_2),y_2)


# Diagnostic plots

# 0
par(mfrow=c(2,2))
plot(y.lm)
plot(y.lm2)
par(mfrow=c(2,4))
plot(y.lm)
plot(y.lm2)
par(mfrow=c(1,1))

# 1
par(mfrow=c(2,2))
plot(y_1.lm)
plot(y_1.lm2)
par(mfrow=c(2,4))
plot(y_1.lm)
plot(y_1.lm2)
par(mfrow=c(1,1))

# 2
par(mfrow=c(2,2))
plot(y_2.lm)
plot(y_2.lm2)
par(mfrow=c(2,4))
plot(y_2.lm)
plot(y_2.lm2)
par(mfrow=c(1,1))


# Plots that show the contribution of idiviadial therms
par(mfrow=c(3,3))

# 0
termplot(y.lm, partial.resid=TRUE, smooth=panel.smooth,
         col.res="gray30")
termplot(y.lm2, partial.resid=TRUE, smooth=panel.smooth,
         col.res="gray30")

# 1
termplot(y_1.lm, partial.resid=TRUE, smooth=panel.smooth,
         col.res="blue")
termplot(y_1.lm2, partial.resid=TRUE, smooth=panel.smooth,
         col.res="blue")

# 2
termplot(y_2.lm, partial.resid=TRUE, smooth=panel.smooth,
         col.res="green")
termplot(y_2.lm2, partial.resid=TRUE, smooth=panel.smooth,
         col.res="green")

summary(y.lm)
summary(y.lm2)
summary(y_1.lm)
summary(y_1.lm2)
summary(y_2.lm)
summary(y_2.lm2)


DAAG::vif(y.lm2)
DAAG::vif(y_1.lm2)
DAAG::vif(y_2.lm2)
