# DAAG EXERCISES

## ex. 11 cap. 3

aberrant_crypt <- c(87, 53, 72, 90, 78, 85, 83)
n <- length(aberrant_crypt)
M <- 10000
crypt_mean <-mean(aberrant_crypt)
crypt_var <- var(aberrant_crypt)

paste("aberrant crypt mean:", crypt_mean)
paste("aberrant crypt variance:", crypt_var)

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

# the histogram shows the absolute difference between means and variances,
# note how the largest part of the samples have this difference equals to zero.
# So, we may conclude that mean and variance are almost the same.

# fisher test
dispersion_ab <- sum((aberrant_crypt - crypt_mean)^2)/crypt_mean
p_value <- pchisq(dispersion_ab, df= n-1, lower.tail= FALSE)
paste("p-value:", p_value)

# the test gives a p-value = 0.056 so we may accept the null hypothesis i.e.
# the aberrant_crypt sample may occur from a Poisson distribution.

## FUNCTIONS ##

chisq_term <- function(o, e)
{
  return ((o-e)*(o-e)/e)
}



## EXERCISE ##

set.seed(101)
K <- 4
bad_friends <- 1
good_friends <- 1
friends <- good_friends + bad_friends
bad_prob <-  c(7/16, 5/16, 3/16, 1/16)
good_prob <- c(1/16, 3/16, 5/16, 7/16)
throws_bad <- rpois(n, 78.28)
throws_good <- aberrant_crypt
pivot = 0.0
for (friend in (1:bad_friends))
{
  pivot <- pivot + sum(chisq_term(table(throws_bad[,friend]), n*bad_prob))
}
pchisq(pivot, df=(K-1)*(bad_friends-1), lower.tail=FALSE)
for (friend in (1:good_friends))
{
  pivot <- pivot + sum(chisq_term(table(throws_good[,friend]), n*bad_prob))
}
pchisq(pivot, df=(K-1)*(friends-1), lower.tail=FALSE)





n <- 50
K <- 4

bad_friends <- 6
good_friends <- 1
friends <- good_friends + bad_friends

bad_prob <-  c(7/16, 5/16, 3/16, 1/16)
good_prob <- c(1/16, 3/16, 5/16, 7/16)


# Simulation (bad friends only)
throws_bad <- replicate(bad_friends, sample(1:K, n, replace=TRUE, prob=bad_prob))

# Simulation (good friends only)
throws_good <- replicate(good_friends, sample(1:K, n, replace=TRUE, prob=good_prob))


pivot = 0.0

# COMPUTE SUMS - BAD FRIENDS

for (friend in (1:bad_friends))
{
  pivot <- pivot + sum(chisq_term(table(throws_bad[,friend]), n*bad_prob))
}

# TEST - BAD-ONLY FRIENDS

pchisq(pivot, df=(K-1)*(bad_friends-1), lower.tail=FALSE)


# COMPUTE SUMS - GOOD FRIENDS

for (friend in (1:good_friends))
{
  pivot <- pivot + sum(chisq_term(table(throws_good[,friend]), n*bad_prob))
}

# TEST - GOOD+BAD FRIENDS

pchisq(pivot, df=(K-1)*(friends-1), lower.tail=FALSE)





## ex. 13 cap. 3

Pb <- matrix(data=c(0.6, 0.2, 0.2, 0.2, 0.4, 0.4, 0.4, 0.3, 0.3), nrow=3, ncol=3)
Pb

Markov <- function(num_iters, start_state, Tp) {
  
  # number of possible states
  num_states <- nrow(Tp)
  
  # stores the states X_t through time
  states  <- numeric(num_iters)
  
  # initialize variable for first state 
  states[1] <- start_state
  
  for(t in 2:num_iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- Tp[states[t-1], ]
    ## draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}

Markov(num_iters=100, start_state=0, Tp= Pb)

#####
library(zoo)
library(lattice)

plotmarkov <-
  function(n=10000, start=1, window=100, transition=Pb, npanels=5){
    xc2 <- Markov(num_iters = n, start_state = start, Tp = transition)
    mav0 <- rollmean(as.integer(xc2==0), window)
    mav1 <- rollmean(as.integer(xc2==0), window)
    npanel <- cut(1:length(mav0), breaks=seq(from=1, to=length(mav0),
                                             length=npanels+1), include.lowest=TRUE)
    df <- data.frame(av0=mav0, av1=mav1, x=1:length(mav0),
                     gp=npanel)
    print(xyplot(av0 + av1 ~ x | gp, data=df, layout=c(1, npanels),
                 type="l", par.strip.text=list(cex=0.65),
                 scales=list(x=list(relation="free"))))
  }

plotmarkov(transition=Pb)

## ex. 6 cap. 4
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

plot(y1_acf[[1]], type="p", pch = 15, max.mfrow=1, ylim=c(-.5,1))
points(y1_acf[[2]]$lag, y1_acf[[2]]$acf, pch = 16, cex = 1.1, col="orange")
points(y1_acf[[3]]$lag, y1_acf[[3]]$acf, pch = 17, cex = 1.2, col="red")

plot(y_acf[[1]], type="p", pch = 15, max.mfrow=1, ylim=c(-.5,1))
points(y_acf[[2]]$lag, y_acf[[2]]$acf, pch = 16, cex = 1.1, col="orange")
points(y_acf[[3]]$lag, y_acf[[3]]$acf, pch = 17, cex = 1.2, col="red")


## ex. 7 cap. 4

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
var_v



# CS EXERCISES

## ex. 3.3 cap. 3

library(stats)

naive_method <- function(n, z){
  zneg_1 <- 0; j_1 <- 1
  for (i in 1:n) {
    if (z[i]<0) {
      zneg_1[j_1] <- z[i]
      j_1 <- j_1 + 1
    }
  }
  return(zneg_1)
}

eff_method <- function(z){
  zneg_2 <- ifelse(z<0, z, NA)
  zneg_2 <- zneg_2[!is.na(zneg_2)]
  j_2 <- length(zneg_2) + 1
  return(zneg_2)
}

n <- 100000; z <- rnorm(n)
naive_time <- system.time(zneg_1 <- naive_method(n, z))
eff_time <- system.time(zneg_2 <- eff_method(z))

comparison <- zneg_1 == zneg_2
paste("elements that are diffent:", which(comparison == FALSE))

print("naive time:"); naive_time
print("eff time:"); eff_time


## ex. 3.5 cap. 3

# a)

set.seed(0); n <- 1000
A <- matrix(runif(n*n),n,n); x.true <- runif(n)
y <- A%*%x.true

# b)

form_inverse <- system.time(AI <- solve(A))
x_1 <- AI %*% y

mean(abs(x_1 - x.true))

# c)

dir_inverse <- system.time(x_2 <- solve(A, y))
mean(abs(x_2 - x.true))

# d)
print("system time to solve Ax = y by performing the A inverse:")
form_inverse

print("system time to solve Ax = y by using directly solve function:")
dir_inverse


# LAB EXERCISES

## es. 1

set.seed(123)
R <- 1000
n <- 10

#generate n values R times
samples <- array(0, c(1, R, n))

for (i in 1:R){
  samples[1, i, ] <- rnorm(n, 0, 1)
}

#compute the sample statistics
samples_stat <- array(0, c(1, 2, R))

#sample mean over R replications 
samples_stat[1, 1, ] <- apply(samples[1, , ], 1 , mean )

#sample variance over R replications
samples_stat[1, 2, ] <- apply(samples[1, , ], 1 , var )

#visualize the results
hist(samples_stat[1, 1, ], breaks= 40, probability = TRUE, 
     xlab="y", main= "N(0,1)", cex.main=1.5)
#overlap the true distribution for the sample mean
curve(dnorm(x, 0, sqrt(1/n)), add = TRUE, col = "red", lwd = 2)

sigma <- 1
par (mfrow=c(1,1), oma=c(0,0,0,0))
hist(samples_stat[1, 2, ], breaks= 40, probability = TRUE, 
     xlab=expression(s^2), main= bquote(s^2), cex.main=1.5)
curve((n-1/sigma^2) * dchisq(x * (n-1/sigma^2), df = n),
      add = TRUE, col="red", lwd=2, main="N(0,1)")

# as we can see from the plot, the empirical shape of the sample variance doesn't
# match the true shape of the variance, so it proof that this estimator for the variance
# is a biased estimator.


## es. 2

# case all players with same probability assumption

set.seed(101)
n <- 50
K <- 4
M <- 6

y <- replicate(M, sample(1:K, n, replace=TRUE, prob=c(7/16, 5/16, 3/16, 1/16)))
dim(y) <- c(M, n)

player <- array(NA, c(1, 4))
for(i in 1:M){
  player<- rbind(player, table(y[i, ]))
}

player <- player[-1, ]
player

chisq.test(player, p = c( 7/16, 5/16, 3/16, 1/16))

# case joined a new greater player

set.seed(101)
n <- 50
K <- 4
M <- 7

y <- replicate(6, sample(1:K, n, replace=TRUE, prob=c(7/16, 5/16, 3/16, 1/16)))
g <- sample(1:K, n, replace=TRUE, prob=c(1/16, 1/16, 9/16, 5/16))
y <- c(y, g)
dim(y) <- c(M, n)

player <- array(NA, c(1, 4))
for(i in 1:M){
  player<- rbind(player, table(y[i, ]))
}

player <- player[-1, ]
player

chisq.test(player, p = c(7/16, 5/16, 3/16, 1/16))

## es. 3

Owners <- c( "Katy Perry", "Justin Bieber", "Taylor Swift", "Cristiano Ronaldo",
             "Kim Kardashian", "Ariana Grande", "Selena Gomez", "Demi Lovato")
Instagram <- c( 69, 98,107, 123, 110, 118, 135, 67)
Twitter <- c( 109, 106, 86, 72, 59, 57, 56, 56)
plot( Instagram, Twitter, pch=21, bg=2, xlim=c(60, 150), ylim=c(40, 120) )
text( Instagram[-6], Twitter[-6]+5, Owners[-6], cex=0.8 )
text( Instagram[6], Twitter[6]-5, Owners[6], cex=0.8 )

n <- length(Twitter)

res <- cor.test(Twitter, Instagram, method = "pearson")
res

# alternative solution
r <- cor(Twitter, Instagram, method = "pearson"); r
t <- r * sqrt((n-2)/(1-r^2)); t
p_value <- 2*pt(-abs(t),df=n-2); p_value

# the p-value is greater than the significance level, so we may accept the null
# hypothesis and conclude that doesn't exist any evidence association between 
# Instagram and Twitter

# es. 4

# need to be solved analitically

# es. 5





