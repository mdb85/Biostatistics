dbinom(x=2, size = 20, prob = 0.4)
1 - pbinom(q=4, size=20, prob = 0.4)
x <- rbinom(5, size=20, prob=0.4) # simule une sequence de 20 lancees, completement aleatoire
mean(x)
barplot(table(x))

# Loi de poisson #
dpois(5, lambda = 4) # Prob d'avoir 5 IAS / mois
dpois(0, lambda = 4) + dpois(1, lambda = 4) # prob d'avoir < 2 IAS / mois
ppois(1, lambda = 4) # prob d'avoir < 2 IAS / mois
1 - ppois(1, lambda = 4) # prob d'avoir >= 2 IAS / mois

# Loi normale #
pnorm(q = 40, mean = 55, sd = 15.5) # prob de femmes avec un taux d'hdl < 40
1 - pnorm(q = 60, mean = 55, sd = 15.5)
rnorm(n=2, mean = 0, sd = 1) # tirer au sort 2 valeurs

# Loi uniforme #
dunif(x = 6.5, min = 4, max = 11)
dunif(x = 12, min = 4, max = 11)
runif(n = 10, min = 4, max = 11) # tirer au sort 10 valeurs

# Calcul d'interval de pari #
qnorm(0.025, mean = 16, sd = sqrt(3/100))
qnorm(0.975, mean = 16, sd = sqrt(3/100))

# for loop #
M <- vector("numeric", length = 1000)
for (i in 1:1000) {
  x <- runif(10, min = 4, max = 11)
  m <- mean(x)
  M[i] <- m
}
hist(M, main = "Histogramme")
lines(density(M), )