library(DAAG)

# load dataset
data <- DAAG::head.injury

# exploring data
str(data) # they are binary data...

# perform glm
model_glm <- glm(clinically.important.brain.injury ~ .,
                 family = binomial(link = "logit"),
                 data = data)

#summary
summary(model_glm)

# turn the result into a decision rule for use of CT
judge_function_1 <- function(patients, threshold){
  n_coeff <- length(model_glm$coefficients) # number of estimated coefficients

  p <- model_glm$coefficients[1] + patients %*% model_glm$coefficients[2:n_coeff]
  logistic_prob <- exp(p) / (1 + exp(p))

  return(logistic_prob < threshold) # return the if condition on the threshold
}

judge_function_2 <- function(patients, threshold){
  logit <-log(threshold/(1 - threshold))
  intercept <- model_glm$coefficients[1]

  CT_data <- data.frame("coeff" = model_glm$coefficients[-1], "patient" = t(patients)); print(CT_data)
  CT_rule <- data.frame("Patient_risk" = patients %*% model_glm$coefficients[-1],
                        "CT_threshold" = logit - intercept,
                        "CT" = patients %*% model_glm$coefficients[-1] > logit - intercept)
  rownames(CT_rule) <- colnames(CT_data[, -1]); print(CT_rule)
}

# number of columns without response column
n <- length(model_glm$coefficients[-1])

# check correctness of the judge function
 x <-  sapply(data,'[[', 1)[-11];  # a patient took from the dataset

# first patient
set.seed(69)
x <- c(x, sample(c(0,1), replace = TRUE, size = n))

# second patient
set.seed(50)
x <- c(x, sample(c(0,1), replace = TRUE, size = n))

# patients table
x <- matrix(x, nrow = 3, ncol = n, byrow = TRUE)

# judge_function_1(patients = x, threshold = 0.025)
judge_function_2(patients = x, threshold = 0.025)
