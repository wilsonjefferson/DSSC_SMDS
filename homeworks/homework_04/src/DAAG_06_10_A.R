dataset <- MPV::table.b3

# exploring data
str(dataset)

par(mfrow=c(1,1))

# scatter plot
plot(dataset$x1, dataset$y, main="Scatterplot x1 versus y",
     xlab="x_1 (displacement)", ylab="y (mpg)", pch=19)
