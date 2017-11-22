mean_ic <- function(xx, alpha=0.05, missing_data=T) {
  mean_value <- mean(xx, na.rm=missing_data)
  size <- sum(!is.na(xx), na.rm=missing_data)
  et <- sd(xx, na.rm=missing_data)
  ealpha <- abs(qnorm(alpha/2))  # epsilon_alpha
  # P(X <= Epslion_alpha) = alpha/2
  # qnorm(alpha/2) == qnorm(1 - alpha/2)
  
  ic_inf <- mean_value - ealpha * et / sqrt(size)
  ic_sup <- mean_value + ealpha * et / sqrt(size)
  
  return (list(mean_value = mean_value, 
               ic_inf = ic_inf,
               ic_sup = ic_sup))
}

tmp <- 1:100
mean_ic(tmp)



n <- 100
mu <- 1
repet <- 10000
vrai <- 0

for (ii in 1:repet) {
  # tirage <- rnorm(n, mean = mu, sd = 1)
  tirage <- rexp(n, rate=1/mu)
  ic_tirage <- mean_ic(tirage)
  vrai <- vrai + (mu < ic_tirage$ic_sup & mu > ic_tirage$ic_inf)
}

vrai / repet

moy_stoc <- NULL
moy_stoc <- c(moy_stoc, 1)