# xy plot
par(mfrow=c(1,1))
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
