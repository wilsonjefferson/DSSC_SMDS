library("lattice")
library("zoo")

set.seed(160898)

# Simulate a discrete MC according to transistion matrix P, given the number of iterations and the initial state
mc_simulation <- function(P, n, initial)
{
  # Number of possible states
  s_states <- nrow(P)

  # States through time
  states <- numeric(n)

  # Initial state
  states[1] <- initial

  for(t in (2:n))
  {
    # Next state
    p <- P[states[t-1],]
    states[t] <- which(rmultinom(1, 1, p) == 1)
  }

  return(states)
}

# Transition matrix
P <- t(matrix(c( 0.6, 0.2, 0.2,
                 0.2, 0.4, 0.4,
                 0.4, 0.3, 0.3 ), nrow = 3, ncol = 3))

# Plot the result
plotmarkov <-
  function(n=1000, start=1, window=100, transition=P, npanels=5){
    xc2 <- mc_simulation(transition, n, start)
    mav1 <- rollmean(as.integer(xc2==1), window)
    mav2 <- rollmean(as.integer(xc2==2), window)
    mav3 <- rollmean(as.integer(xc2==3), window)
    npanel <- cut(1:length(mav1), breaks=seq(from=1, to=length(mav1),
                                             length=npanels+1), include.lowest=TRUE)
    df <- data.frame(av1=mav1, av2=mav2, av3=mav3, x=1:length(mav1),
                     gp=npanel)
    print(xyplot(av1+av2+av3 ~ x | gp, data=df, layout=c(1,npanels),
                 type="l", par.strip.text=list(cex=0.65),
                 scales=list(x=list(relation="free"))))
  }

# Somehow a starting point:
plotmarkov(1000, 1, 200, P, 1)

# Changing window size:
plotmarkov(1000, 1, 500, P, 1)

# Changing overall number of states:
plotmarkov(2000, 1, 200, P, 1)

# Changing window size:
plotmarkov(2000, 1, 500, P, 1)

# Changing overall number of states:
plotmarkov(3000, 1, 200, P, 1)

# Changing window size:
plotmarkov(3000, 1, 500, P, 1)

# Changing window size:
plotmarkov(3000, 1, 900, P, 1)

# Changing overall number of states:
plotmarkov(4000, 1, 200, P, 1)

# Changing window size:
plotmarkov(4000, 1, 500, P, 1)

# Changing window size:
plotmarkov(4000, 1, 900, P, 1)

# Changing both:
plotmarkov(5000, 1, 1000, P, 1)

# Changing both:
plotmarkov(10000, 1, 2000, P, 1)

# Five-panel plot of different chains
plotmarkov(10000, 1, 1000, P, 5)
