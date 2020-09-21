# fit models according the group x11: y = beta_0 + beta_1 * x7 + eps
fitted_model_x7_1 <- lm(y ~ x7, data = dataset[dataset$x11 == 0, ])
fitted_model_x7_2 <- lm(y ~ x7, data = dataset[dataset$x11 == 1, ])

# residuals plots group x11 = 0
par(mfrow=c(2, 2))
plot(fitted_model_x7_1, main = "Analisys for model y ~ x7 (x11 = 0)")

# the residuals are spreaded in just three integer values that are all
# multiple of three, moreover the qq-plot shows how the normality 
# assumption is quite weak.

# residuals plots group x11 = 1
par(mfrow=c(2, 2))
plot(fitted_model_x7_2, main = "Analisys for model y ~ x7 (x11 = 1)")

# x7 is the same for all points in group x11 = 1, then they have the
# same fitted value, the scal-location plot is not reasonable and the
# qqplot is quite good but just because assumed the same value.

# fitted model plots
par(mfrow=c(1, 2))
plot_fit(fitted_model_x7_1, 
         dataset[dataset$x11 == 0, ]$x7, 
         dataset[dataset$x11 == 0, ]$y,
         xlab = "x7", 
         subtitle = "group x11 = 0")

plot_fit(fitted_model_x7_2, 
         dataset[dataset$x11 == 1, ]$x7, 
         dataset[dataset$x11 == 1, ]$y,
         xlab = "x7", 
         subtitle = "group x11 = 1")

# x7 versus residuals
par(mfrow=c(1,2))
plot(dataset[dataset$x11 == 0, ]$x7, 
     fitted_model_x7_1$residuals, main="Scatterplot x7 versus Residuals",
     xlab="x7", ylab="Residuals", pch=19) 

plot(dataset[dataset$x11 == 1, ]$x7, 
     fitted_model_x7_2$residuals, main="Scatterplot x7 versus Residuals",
     xlab="x7", ylab="Residuals", pch=19)
