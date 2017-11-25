require(survival)

data(pbc)
?pbc

pbc2 <- pbc[(!is.na(!pbc$spiders)) & (!is.na(pbc$stage)), ]
pbc2$M <- as.numeric(pbc2$stage == 4)

# Se
sensibilite <- sum(pbc2$M & pbc2$spiders) / sum(pbc2$M)

# Sp
specificite <- sum(pbc2$M==0 & pbc2$spiders==0) / sum(pbc2$M == 0)

## Courbe ROC pour le diagnostic de cirrhose (stage=4)
## par mesure de temps de coagulation (protime)
pbc3 <- pbc[(!is.na(!pbc$spiders)) & (!is.na(pbc$stage)), ]
pbc3$M <- as.numeric(pbc3$stage == 4)

sens = spec = seuil = NULL

for (ci in (seq(min(pbc3$protime)-0.1, max(pbc3$protime)+0.1, 0.1))) {
  
  # Se
  se_ci <- sum(pbc3$M==1 & (pbc3$protime>ci)) / sum(pbc3$M)
  
  # Sp
  sp_ci <- sum(pbc3$M==0 & (pbc3$protime<=ci)) / sum(pbc3$M == 0)
  
  sens <- c(sens, se_ci)
  spec <- c(spec, sp_ci)
  seuil <- c(seuil, ci)
}

## SE et SP en fonction du seuil
plot(x=seuil, y=sens, type="l", xlab="ProTime", ylab="Se/Sp")
lines(x=seuil, y=spec, type="l", col=4)

## Courbes ROC
plot(x=1-spec, y=sens, type="s", xlab="1-Sp", ylab="Se")
abline(a=0, b=1, lty=2)

## package proc
require(pROC)
plot(roc(pbc3$M, pbc3$protime))