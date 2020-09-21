# LIBRARIES
library(lattice)
library(DAAG)


# TABLE
rbind(Number = table(moths[,4]), sapply(split(DAAG::moths[,-4], DAAG::moths$habitat),
                                        apply, 2, sum))


# IND. TRANSECTS / MEANS
lattice::dotplot(habitat~P, data=DAAG::moths, xlab="Number of moths (species P)",

    panel = function(x, y, ...)
    {
        lattice::panel.dotplot(x, y, pch = 1, col = "black", ...)
        lattice::panel.average(x, y, pch = 3, cex = 1.25, type = "p", col = "gray45")
    },

    key = list(text = list(c("Individual transects", "Mean")),
               points  = list(pch = c(1,3), cex = c(1,1.25), col = c("black", "gray45")),
               columns = 2))


# QUASIPOISSON GLM, BANK REFLEVEL
pmoths_qpois <- glm(P ~ habitat + log(meters), family = quasipoisson, data = moths)
summary(pmoths_qpois)


# Analysis with tighter convergence criterion: omitted as no 0-count observed!


# QUASIPOISSON GLM, UPPERSIDE REFLEVEL (Pareto-symplex optimal significance)
moths$habitat_rel <- relevel(DAAG::moths$habitat, ref="Upperside")
pmoths_qpois_rel  <- glm(P ~ habitat_rel + log(meters), family = quasipoisson, data = moths)
summary(pmoths_qpois_rel)



# THE ACTUAL EXERCISE:

# (a): POISSON GLM, UPPERSIDE REFLEVEL
moths$habitat_rel <- relevel(DAAG::moths$habitat, ref="Upperside")
pmoths_pois_rel  <- glm(P ~ habitat_rel + log(meters), family = poisson, data = moths)
summary(pmoths_pois_rel)    # Shrinkage of C.I.s as expected!

# (b): QUASIPOISSON GLM, UPPERSIDE REFLEVEL
moths$habitat_rel <- relevel(DAAG::moths$habitat, ref="Upperside")
pmoths_qpois_rel  <- glm(P ~ habitat_rel + log(meters), family = quasipoisson, data = moths)
summary(pmoths_qpois_rel)   # Significant transect length!


# Distinguishability analysis with deviances: omitted as no "problematic" habitat!
