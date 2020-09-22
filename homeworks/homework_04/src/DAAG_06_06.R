# LIBRARIES:
library(lattice)
library(DAAG)

# DATA:
data(nihills)


### ORIGINAL DATASET ###


# Fit models
nihills_lm1 <- lm(time ~ dist+climb,            data=nihills)
nihills_lm2 <- lm(time ~ dist+climb+dist:climb, data=nihills)

# ANOVA
anova(nihills_lm1, nihills_lm2)

# Diagnostic plots
par(mfrow=c(2,2))
plot(nihills_lm1)

par(mfrow=c(2,2))
plot(nihills_lm2)


### ONE PROBLEMATIC DATAPOINT REMOVED ###


# Removal
rows_to_remove <- c("Seven Sevens")
nihills_pruned <- nihills[!(row.names(nihills) %in% rows_to_remove),]


# Fit models
nihills_lm1_pruned <- lm(time ~ dist+climb,            data=nihills_pruned)
nihills_lm2_pruned <- lm(time ~ dist+climb+dist:climb, data=nihills_pruned)

# ANOVA
anova(nihills_lm1_pruned, nihills_lm2_pruned)

# Diagnostic plots
par(mfrow=c(2,2))
plot(nihills_lm1_pruned)

par(mfrow=c(2,2))
plot(nihills_lm2_pruned)


### TWO PROBLEMATIC DATAPOINTS REMOVED ###


# Removal
rows_to_remove      <- c("Seven Sevens", "Annalong Horseshoe")
nihills_pruned_more <- nihills[!(row.names(nihills) %in% rows_to_remove),]


# Fit models
nihills_lm1_pruned_more <- lm(time ~ dist+climb,            data=nihills_pruned_more)
nihills_lm2_pruned_more <- lm(time ~ dist+climb+dist:climb, data=nihills_pruned_more)

# ANOVA
anova(nihills_lm1_pruned_more, nihills_lm2_pruned_more)

# Diagnostic plots
par(mfrow=c(2,2))
plot(nihills_lm1_pruned_more)
