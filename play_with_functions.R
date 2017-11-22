variance <- function(xx) {
  size <- length(xx)
  biased_estimator = sum((xx - mean(xx)) ^ 2) / size
  unbiased_estimator = (size * biased_estimator) / (size - 1)
  
  return (list(biased_estimator=biased_estimator,
               unbiased_estimator=unbiased_estimator))
}

require(survival)
data(pbc)

summary(pbc)

variance(pbc$age)