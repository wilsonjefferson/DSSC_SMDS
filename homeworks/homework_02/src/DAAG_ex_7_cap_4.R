corr <- function(){
  y1 <- rnorm(51)
  y <- y1[-1] + y1[-51]
  return(y)
}

computation <- replicate(25, corr(), simplify=FALSE)

av <- c()
v <- c()
for(i in 1:25){
  av <- c(av, mean(computation[[i]]))
  v <- c(v, var(computation[[i]]))
}

var_v <- var(av)
paste("variance:", var_v)
