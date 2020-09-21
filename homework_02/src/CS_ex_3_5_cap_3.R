# Part: (a)
set.seed(0); n <- 1000
A <- matrix(runif(n*n),n,n); x.true <- runif(n)
y <- A%*%x.true

# Part: (b)
form_inverse <- system.time(AI <- solve(A))       # Nested ass't (some linters may complain)
x_1 <- AI %*% y

mean(abs(x_1 - x.true))

# Part: (c)
dir_inverse <- system.time(x_2 <- solve(A, y))    # Nested ass't (some linters may complain)
mean(abs(x_2 - x.true))

# Part: (d)
print("system time to solve Ax = y by performing the A inverse:")
form_inverse

print("system time to solve Ax = y by using directly solve function:")
dir_inverse
