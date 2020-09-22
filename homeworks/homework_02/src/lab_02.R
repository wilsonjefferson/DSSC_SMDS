## FUNCTIONS ##

chisq_term <- function(o, e)
{
  return ((o-e)*(o-e)/e)
}



## EXERCISE ##

set.seed(1023)

# Observation nr., friends, zones, probabilities

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
