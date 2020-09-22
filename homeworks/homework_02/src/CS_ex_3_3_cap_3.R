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
naive_time <- system.time(zneg_1 <- naive_method(n, z))  # Nested ass't (some linters may complain)
eff_time <- system.time(zneg_2 <- eff_method(z))         # Nested ass't (some linters may complain)

# Check for equality
comparison <- zneg_1 == zneg_2
paste("Elements that are diffent: ", which(comparison == FALSE))

# Benchmark
print("Naive time:"); naive_time
print("Efficient time:"); eff_time
