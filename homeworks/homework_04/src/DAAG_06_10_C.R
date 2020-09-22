# fit models according the group x11: y = beta_0 + beta_1 * x1 + eps
fitted_model_x1_1 <- lm(y ~ x1, data = dataset[dataset$x11 == 0, ])
fitted_model_x1_2 <- lm(y ~ x1, data = dataset[dataset$x11 == 1, ])

# diagnostics
summary(fitted_model_x1_1) # group x11 = 0
summary(fitted_model_x1_2) # group x11 = 1

# residuals plots group x11 = 0
par(mfrow=c(2, 2))
plot(fitted_model_x1_1, main = "Analisys for model y ~ x1 (x11 = 0)")

# residuals plots group x11 = 1
par(mfrow=c(2, 2))
plot(fitted_model_x1_2, main = "Analisys for model y ~ x1 (x11 = 1)")

# prepare the modified model without point 5
dataset_b <- dataset[-5, ]
fitted_model_x1_1_b <- lm(y ~ x1, data = dataset_b[dataset_b$x11 == 0, ])

# fitted model plots
par(mfrow=c(1, 3))
plot_fit(fitted_model_x1_1, 
         dataset[dataset$x11 == 0, ]$x1, 
         dataset[dataset$x11 == 0, ]$y, 
         xlab = "x1", 
         subtitle = "group x11 = 0")

# the plot of the fitted model shows us that the linear regressor could be a 
# reasonable regressor, moreover the observation with large x1 could be the 
# critical point of the residual vs laverage analysis. Check if it is an
# outlier and/or an influencer by looking the next plot

plot_fit(fitted_model_x1_1_b, 
         dataset_b[dataset_b$x11 == 0, ]$x1, 
         dataset_b[dataset_b$x11 == 0, ]$y, 
         xlab = "x1", 
         subtitle = "group x11 = 0 (without point 5)")

# by looking the plot it's clear that point 5 is almost an influencer point.

plot_fit(fitted_model_x1_2, 
         dataset[dataset$x11 == 1, ]$x1, 
         dataset[dataset$x11 == 1, ]$y,
         xlab = "x1", 
         subtitle = "group x11 = 1")

# the fitted model looks quite good!
