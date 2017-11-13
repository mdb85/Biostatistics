obs <- runif(100, min=2, max=25)
var <- (25 - 2)^2/12 # var de x
s2 <- var(obs) # estimation
q2 <- 1/100 * sum((obs - mean(obs))^2) # variance expimentale

s2 <- vector("numeric", length = 10^5)
q2 <- vector("numeric", length = 10^5)

for (i in 1:10^5) {
  obs <- runif(100, min=2, max=25)
  s2[i] <- var(obs)
  q2[i] <- 1/100 * sum((obs - mean(obs))^2)
}

# Biais estimateur experimentale
biais1 <- mean(q2) - variance

# Biais estimaeur variance
biais2 <- mean(s2) - variance

library("MASS")
data("birthwt")
dim(birthwt)
summary(birthwt)

summary(birthwt$bwt)


hist(x=birthwt$bwt, probability = TRUE, main="Poids de naissance")

# Estimation non biaisee
s2 <- var(birthwt$bwt)

# var expe
n <- length(!is.na(birthwt$bwt))

q2 <- 1/n*sum((birthwt$bwt-mean(birthwt$bwt))^2)

## IC
xbar <- mean(birthwt$bwt, na.m=TRUE)
epsilon <- qnorm(1-0.05/2)

borneSup <- xbar + epsilon*sqrt(s2/n)
borneInf <- xbar - epsilon*sqrt(s2/n)

ic <- function(x, alpha) {
  m <- mean(x, na.rm=TRUE)
  s2 <- var(x, na.rm=TRUE)
  n <- length(!is.na(x))
  epsilon <- qnorm(1-alpha/2)
  borneSup <- xbar + epsilon*sqrt(s2/n)
  borneInf <- xbar - epsilon*sqrt(s2/n)
  return (c(m, borneInf, borneSup))
}

ic(birthwt$bwt, alpha=0.05)
ic(birthwt$age, alpha=0.05)

result <- matrix(NA, nrow=10000, ncol=3)
for (i in 1:10000) {
  obs <- rnorm(n=189, mean=3000, sd=730)
  result[i,] <- ic(obs, alpha=0.05)
}

nrow(result[result[,2] < 3000 & result[, 3] > 3000 ,])/nrow(result)

# cas ou n est ptit #

# proportion de petit poids de naissance (variable low) chez les meres hypertendues
table(birthwt$low[birthwt$ht==1])
table(birthwt$ht)

binom.test(n=12, x=7) ## ne marche que pour les proportions, pas pou les moyennes

# poids de naissance moyen chez les hypertendues n=12

# On suppose poids de naissance suit une loi normale
m <- mean(birthwt$bwt[birthwt$ht==1])
s <- sd(birthwt$bwt[birthwt$ht==1])
n < 12

icbi<- m+qt(0.025, 11)*s/sqrt(n)
icbs<- m+qt(0.975, 11)*s/sqrt(n)