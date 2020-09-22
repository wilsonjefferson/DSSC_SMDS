set.seed(1)

y1_acf <- list()
y_acf <- list()

par(mfrow=c(2, 1))
for(i in 1:3){
  y1 <- rnorm(51)
  y <- y1[-1] + y1[-51]

  y1_acf[[i]] <- acf(y1, na.action=na.pass, plot=FALSE)
  y_acf[[i]] <- acf(y, na.action=na.pass, plot=FALSE)
}

plot(y1_acf[[1]], type="p", pch = 15, max.mfrow=1, ylim=c(-.5,1.1))
points(y1_acf[[2]]$lag, y1_acf[[2]]$acf, pch = 16, cex = 1.1, col="orange")
points(y1_acf[[3]]$lag, y1_acf[[3]]$acf, pch = 17, cex = 1.2, col="red")

plot(y_acf[[1]], type="p", pch = 15, max.mfrow=1, ylim=c(-.5,1))
points(y_acf[[2]]$lag, y_acf[[2]]$acf, pch = 16, cex = 1.1, col="orange")
points(y_acf[[3]]$lag, y_acf[[3]]$acf, pch = 17, cex = 1.2, col="red")
