## EXERCISE 5 ##

#true mean
theta_sample <- 2
#likelihood variance
sigma2 <- 2
#sample size
n <- 10
#prior mean
mu <- 7
#prior variance
tau2 <- 2

#generate some data
set.seed(123)
y <- rnorm(n,theta_sample, sqrt(sigma2))

#posterior mean
mu_star <- ((1/tau2)*mu+(n/sigma2)*mean(y))/( (1/tau2)+(n/sigma2))
#posterior standard deviation
sd_star <- sqrt(1/( (1/tau2)+(n/sigma2)))

# No conjugate Prior
library(rstan)
#launch Stan model
data<- list(N=n, y=y, sigma =sqrt(sigma2), mu = mu, tau = sqrt(tau2))
fit <- stan(file="/home/pietro/Documents/Statistical_method_data_science/smds_exercises/Lab/normal.stan",
            data = data, chains = 4, iter=2000)

#extract Stan output
sim <- extract(fit)

# QQPlot
library("car")

quantile(sim$theta)
boxplot(sim$theta, horizontal = TRUE, col = "lightgray", main = "Theta Box Plot")

"
from the numerical value of the quantiles it seems the theta distribution
is well distribuited around the mean, infact the quantiles are at the same
distance from it. Moreover it is immediately clear by looking on the box plot:
the various theta values are, more or less, simmetrically spreaded around the mean.
"

qqnorm(sim$theta, pch = 1, frame = FALSE, main = "Normal Q-Q Plot for theta")
qqline(sim$theta, col = "steelblue", lwd = 2)

"
the Q-Q plot shows us what suppose until know, the theta distribution is distribuited
like a normal distribution, even if a there is a tiny variability on the tails. So, we
may accept the normality assumption.
"

# plot of the simulated posterior and the true posterior
hist(sim$theta, probability = TRUE, main = "Theta's Histogram", xlab = "theta")
curve(dnorm(x, mu_star, sd_star), 
     xlab=expression(theta), ylab="", col="blue", lwd=2,
     cex.lab=2, add=T)

## EXERCISE 6 ##

library(bayesplot)
posterior <- as.array(fit)

# numerical intervals
mcmc_intervals_data(posterior)[1, ]

# numerical areas
mcmc_areas_data(posterior)

# posterior distribution plot
plot_title <- ggtitle("Marginal Posterior distribution for theta", "with median and 80% interval")
mcmc_areas(posterior, pars = c("theta"), prob = 0.8) + plot_title

"
The bayes plot library provide us a posterior-plot of the our posterior, 
and it shows us an approximately normal distribution centered in 2.6 for the theta parameter.
"

# interesting things

# density plot
mcmc_hex(posterior)

# autocorrelation plot
mcmc_acf(posterior)

"
The acf shows us how the parameter is basically no-autocorrelated because
the ac curve is quite close to zero in all chains.
"

# rhat analysis
rhats <- rhat(fit)
mcmc_rhat(rhats)+ yaxis_text(hjust = 1)

"
The rhat is quite close to 1, that is good for convergence purposes.
"

# neff analysis
ratios <- neff_ratio(fit)
mcmc_neff(ratios, size = 3) + yaxis_text(hjust = 1)

"
From the plot analysis we see that the neef of the parameter is not so high.
"

# combo plot
combo_plot <- c("areas", "trace", "hist", "hex")
color_scheme_set("mix-blue-red")
mcmc_combo(
  posterior,
  pars = c("theta", "lp__"),
  combo = combo_plot,
  gg_theme = ggplot2::theme_gray() + legend_none()
)

## EXERCISE 7 ##

#input values


#true mean
theta_sample <- 2
#likelihood variance
sigma2 <- 2
#sample size
n <- 15
#prior mean
mu <- 7
#prior variance
tau2 <- 2

#generate some data
set.seed(123)
y <- rnorm(n,theta_sample, sqrt(sigma2))

#posterior mean
mu_star <- ((1/tau2)*mu+(n/sigma2)*mean(y))/( (1/tau2)+(n/sigma2))
#posterior standard deviation
sd_star <- sqrt(1/( (1/tau2)+(n/sigma2)))


curve(dnorm(x, theta_sample, sqrt(sigma2/n)),xlim=c(-4,15), lty=2, lwd=1, col="black", ylim=c(0,1.4), 
      ylab="density", xlab=expression(theta))

curve(dnorm(x, mu, sqrt(tau2) ), xlim=c(-4,15), col="red", lty=1,lwd=2,  add =T)
curve(dnorm(x, mu_star, sd_star), 
      xlab=expression(theta), ylab="", col="blue", lwd=2, add=T)  
legend(8.5, 0.7, c("Prior", "Likelihood", "Posterior"), 
       c("red", "black", "blue", "grey" ), lty=c(1,2,1),lwd=c(1,1,2), cex=1)




















## EXERCISE 8 ##

library("bayesplot")
library("rstanarm")
library("ggplot2")



#MCMC areas
posterior <- as.matrix(fit2)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(posterior, 
           pars = "theta", 
           prob = 0.8) + plot_title

#launch biparametric Stan model

data3<- list(N=n, y=y, a=-10, b=10)
fit3 <- stan(file="biparametric.stan", data = data3, chains = 4, iter=2000,
             refresh=-1)

#extract stan output for biparametric model

sim3 <- extract(fit3)
posterior_biv <- as.matrix(fit3)

theta_est <- mean(sim3$theta)
sigma_est <- mean(sim3$sigma)
c(theta_est, sigma_est)
traceplot(fit3, pars=c("theta", "sigma"))

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(posterior_biv, 
           pars = c("theta","sigma"), 
           prob = 0.8) + plot_title
