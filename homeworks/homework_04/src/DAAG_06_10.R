## CAP. 10 - EX. 10

library(MPV)
library(lattice)

dataset <- MPV::table.b3

# exploring data
str(dataset)
summary(dataset)

plot_fit <- function(fitted_model, x, y, xlab, subtitle){
    plot(x, y, main = "fitted model", xlab = xlab, sub = subtitle)
  lines(x, as.vector(fitted_model$fitted.values), col ="red")
  text(13,3, "y = b0+b1*x", col="red")
}

## a

# scatter plot
par(mfrow = c(1, 1))
plot(dataset$x1, dataset$y, main="Scatterplot x1 versus y",
     xlab="x_1 (displacement)", ylab="y (mpg)", pch=19)

# just by looking on the scatter plot we may suppose a non linear relationship
# between the output y and the predictor x1: these variables seems to be negative
# correlated (as x1 increase, y decrease) according to a convex smoothed curve.
# We may consider some possible transformation.

# Not strange points are (yet) detected.

## b

# xy plot
lattice::xyplot(y~x1,data=dataset,
       groups = x11,
       pch = 19,
       xlab="x1",
       ylab="y",
       main="Splitted Scatter Plot",
       col = c("green", "blue"),
       key=list(space="right",
                lines=list(col=c("green","blue"), lty=c(3,2), lwd=6),
                text=list(c(" group x11 = 0"," group x11 = 1"))
       ))

# just by looking on the plot, we may suppose that the data might be
# well divided according to the trasmission method (column x11), infact
# the observation belonging to x11 = 0 are in the x1 range [100, 200]. While,
# the others belong to the range [200, 500].

# Even if the dataset is splitted in two subset, the kind of relationship between
# y and x1 is still visible, but this time we may suppose a high relation for
# the group x1 = 0  than the other group.

# In the end, according to the split, we may see that now is reasonable to
# consider the two models like a linear models.

# Not strange points are (yet) detected for both models.

## c

# fit models according the group x11: y = beta_0 + beta_1 * x1 + eps
fitted_model_x1_1 <- lm(y ~ x1, data = dataset[dataset$x11 == 0, ])
fitted_model_x1_2 <- lm(y ~ x1, data = dataset[dataset$x11 == 1, ])

# diagnostics
summary(fitted_model_x1_1) # group x11 = 0

# residuals: they are spreaded in a quite large range and they seem
#            to be not so symmetrical (more concentrated on the
#            left side of the median).
# x1       : the estimate is the slope of the linear regressor and it's
#            a negative value but quite close to zero (so the slope of the
#            line is not so marked). Moreover, the t statistic is, in module,
#            quite small but the p-value still suggests to reject the null
#            hypothesis agains the estimate.
# R^2 adj  : the variability of the model is explained with the 67% just by
#            considering the x1 predictor, we may do better by looking on
#            other predictors.
# F-stat   : it's value is not so far from 1 but the p-value suggests us to
#            reject the null hypothesis against all the coefficients estimated
#           (without considering the intercept).

# In conclusion, we may accept this simple linear regressor, but taking into
# account that it does not be able to explain very well the model.

summary(fitted_model_x1_2) # group x11 = 1

# residuals: they are spreaded in a resonable range and they seem
#             to be symmetrical.
# x1       : the estimate is the slope of the linear regressor and it's
#            a negative value but quite close to zero (so the slope of the
#            line is not so marked). Moreover, the t statistic is, in module,
#            quite small but the p-value still suggests to reject the null
#            hypothesis agains the estimate.
# R^2 adj  : the variability of the model is explained with the 58% just by
#            considering the x1 predictor, we may do better by looking on
#            other predictors.
# F-stat   : it's value is not so far from 1 but the p-value suggests us to
#            reject the null hypothesis against all the coefficients estimated.
#           (without considering the intercept).

# In conclusion, we may accept this simple linear regressor, but taking into
# account that it does not be able to explain very well the model.

# residuals plots group x11 = 0
par(mfrow=c(2,2))
plot(fitted_model_x1_1, main = "Analisys for model y ~ x1 (x11 = 0)")

# residual vs fitted: the plot suggest us a kind of pattern among the residuals
#                     but the observation used to fit the model are not so much
#                     and they look quite spread, so we cannot infere on the
#                     non-linear relationship among the residuals of the model.
# qq plot           : the residuals seem to be normal distributed even if the
#                     point 15 is much far from the straight line (could be an
#                     outlier or a point with high level of information).
# scale-location    : the red line suggests us heteroskedasticity for the residuals
#                     but, inasmuch the number of observations is small, we cannot
#                     accept the heteroskedasticity
# residual vs lvg   : the point 5 is in the critical region of the plot and it means
#                     that it has a strong influence on the model, so it could be an
#                     outlier or an influencer.

# The point 5 seems to be both an outlier and an influencer because its cook distance
# is very large, moreover its residual is significaly far from zero, in the end it's
# the only pointed fittend in the first portion of the model.

# prepare the modified model without point 5
dataset_b <- dataset[-5, ]
fitted_model_x1_1_b <- lm(y ~ x1, data = dataset_b[dataset_b$x11 == 0, ])

# residuals plots group x11 = 1
par(mfrow=c(2,2))
plot(fitted_model_x1_2, main = "Analisys for model y ~ x1 (x11 = 1)")

# residual vs fitted: the plot suggest no clear pattern among the residuals, and
#                     we may suppose a non-linear relationship on the residuals.
# qq plot           : the residuals seem to be not so much normal distributed
#                     just by looking on the point outside the range [-1, 1], so the
#                     normality assumption is quite weak.
# scale-location    : the red line suggests us homoskedasticity for the residuals
#                     that might be correct, and we may accept it.
# residual vs lvg   : no one points is in the critical region, even if the point 17
#                     is close to the borderline, it could be a very influence point.

# fitted model plots
par(mfrow=c(1,3))
plot_fit(fitted_model_x1_1,
         dataset[dataset$x11 == 0, ]$x1,
         dataset[dataset$x11 == 0, ]$y,
         xlab = "x1",
         subtitle = "group x11 = 0")

# the plot of the fitted model shows us that the linear
# regressor could be a  reasonable regressor

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

###

# fit models according the group x11: y = beta_0 + beta_1 * x7 + eps
fitted_model_x7_1 <- lm(y ~ x7, data = dataset[dataset$x11 == 0, ])
fitted_model_x7_2 <- lm(y ~ x7, data = dataset[dataset$x11 == 1, ])

# diagnostics
summary(fitted_model_x7_1) # group x11 = 0
summary(fitted_model_x7_2) # group x11 = 1

# residuals plots group x11 = 0
par(mfrow=c(2,2))
plot(fitted_model_x7_1, main = "Analisys for model y ~ x7 (x11 = 0)")

# residuals plots group x11 = 1
par(mfrow=c(2,2))
plot(fitted_model_x7_2, main = "Analisys for model y ~ x7 (x11 = 1)")

# fitted model plots
par(mfrow=c(1,2))
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

## d

# x7 versus residuals
par(mfrow=c(1,2))
plot(dataset[dataset$x11 == 0, ]$x7,
     fitted_model_x7_1$residuals, main="Scatterplot x7 versus Residuals",
     xlab="x7", ylab="Residuals", pch=19)

plot(dataset[dataset$x11 == 1, ]$x7,
     fitted_model_x7_2$residuals, main="Scatterplot x7 versus Residuals",
     xlab="x7", ylab="Residuals", pch=19)

# The residual plot for the group x11 = 0 shows us that the points are
# spread just on three values of x7 (integer values) and seem doesn't
# exist a clear pattern among them.

# While in the residual plot for the group x11 = 1, the points are all
# distributed on the single value of x7, so it might suggest that x7 is
# not a suitable predictor for our model because we have multiple output
# for the same value of x7: we may discard it.

