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

# Trasition matrix
P <- t(matrix(c( 0.6, 0.2, 0.2,
                 0.2, 0.4, 0.4,
                 0.4, 0.3, 0.3 ), nrow = 3, ncol = 3))

# Number of calculated states
n_iter <- 1000

# Simulate chain
MC <- numeric(n_iter)
MC <- mc_simulation( P, n_iter, 1 )

result <- t(matrix(c("Sun", "Cloud", "Rain",
                     table(MC)/n_iter), nrow = 3, ncol = 2))

# Print the result
print(result)


n_iter <- 1000000

# Simulate chain
MC <- numeric(n_iter)
MC <- mc_simulation( P, n_iter, 1 )

result <- t(matrix(c("Sun", "Cloud", "Rain",
                     table(MC)/n_iter), nrow = 3, ncol = 2))

# Print the result
print(result)
