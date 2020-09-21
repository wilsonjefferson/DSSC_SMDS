# LIBRARIES
library(ggplot2)

# DATA:
conc <- c(0.1, 0.5, 1.0, 10.0, 20.0, 30.0, 50.0, 70.0, 80.0, 100.0, 150.0)
no   <- c(7,   1,  10,    9,    2,    9,   13,    1,    1,     4,     3  )
yes  <- c(0,   0,   3,    4,    0,    6,    7,    0,    0,     1,     7  )


# COMPOSITES
yes_probab <- yes/(yes+no)
no_probab  <- no/(yes+no)


# Exploratory plot:

plot(conc, yes_probab)      # No obvious pattern...
plot(conc, no_probab)       # ... emerges in the data! :(


# Blind-fitting with total-count weighting

fit_out <- glm(yes_probab ~ conc,                # At least yes_probab it's increasing on average
               family=binomial(link="logit"),
               weights = (yes+no)                # and let the magic happen!
               )


synthdata <- data.frame(conc = seq(min(conc), max(conc), len=100))
synthdata$yes_probab = predict(fit_out, newdata = synthdata, type="response")

plot(yes_probab~conc)
lines(yes_probab~conc, synthdata)
