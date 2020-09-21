aberrant_crypt <- c(87, 53, 72, 90, 78, 85, 83)
n <- length(aberrant_crypt)
M <- 10000

crypt_mean <-mean(aberrant_crypt)
crypt_var <- var(aberrant_crypt)

paste("Aberrant crypt foci mean (sample):", crypt_mean)
paste("Aberrant crypt foci variance (sample):", crypt_var)

vec_mean <- c()
vec_var <- c()
for (i in 0:M-1){
  x <- rpois(n, 78.28)
  vec_mean <- c(vec_mean, mean(x))
  vec_var <- c(vec_var, var(x))
}

hist(x = abs(vec_mean -vec_var),
     main = "Difference between mean and variance",
     xlab = "|mean - var|",
     ylab = "frequency")

# Fisher Dispersion Test for Poissonity (1950)
dispersion_ab <- sum((aberrant_crypt - crypt_mean)^2)/crypt_mean
p_value <- pchisq(dispersion_ab, df= n-1, lower.tail= FALSE)
paste("p-value:", p_value)
