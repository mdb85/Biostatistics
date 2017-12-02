base <- read.csv2("~/../Desktop/Biostatistics/base_projet1.csv")
base$SEXE
summary(base)
length(base$SEXE)
base[base$SIDA == 1,]$DEPARTEMENT

length(base[base$SIDA == 1,]$SIDA)
length(base[base$INSUF_CARD == 1,]$INSUF_CARD)
length(base[base$ATTEINTE_HEPATIQUE == 1,]$ATTEINTE_HEPATIQUE)
length(base[base$TUMEUR == 1,]$TUMEUR)
length(base[base$CANCER_METASTATISTIQUE == 1,]$CANCER_METASTATISTIQUE)
length(base[base$ADMISSION_TRAUMA == 1,]$ADMISSION_TRAUMA)
length(base[base$ADMISSION_CHIR == 1,]$ADMISSION_CHIR)
length(base[base$ADMISSION_URGENT == 1,]$ADMISSION_URGENT)
length(base[base$ADMISSION_URGENT == 1,]$ADMISSION_URGENT)
length(base[base$TRAITEMENT == "A",]$TRAITEMENT)

length(base[base$DECES == 1,]$DECES)/500
base$DEPARTEMENT
sort(unique(base$DEPARTEMENT))

depp <- 95
length(base[base$DEPARTEMENT == depp,]$DEPARTEMENT)
length(base[base$DEPARTEMENT == depp,]$DEPARTEMENT) / 500

unique(base$SOFA_INIT)
unique(base$SAPS)





temp <- base$TEMPERATURE[!is.na(base$TEMPERATURE)]
temp_m <- median(temp)
temp_quantile <- quantile(temp, probs=c(0.25, 0.75))
sd(temp)

kk <- base$K[!is.na(base$K)]
kk_m <- median(kk)
kk_quantile <- quantile(kk, probs=c(0.25, 0.75))
sd(kk)

bili <- base$BILIRUBINE[!is.na(base$BILIRUBINE)]
bili_m <- median(bili)
bili_quantile <- quantile(bili, probs=c(0.25, 0.75))
sd(bili)

uree <- base$UREE[!is.na(base$UREE)]
uree_m <- median(uree)
quantile(uree, probs=c(0.25, 0.75))
sd(uree)

ht <- base$HT[!is.na(base$HT)]
median(ht)
quantile(ht, probs=c(0.25, 0.75))
sd(ht)

wbc <- base$WBC[!is.na(base$WBC)]
median(wbc)
quantile(wbc, probs=c(0.25, 0.75))
sd(wbc)

pam <- base$PAM[!is.na(base$PAM)]
median(pam)
quantile(pam, probs=c(0.25, 0.75))
sd(pam)

fc <- base$FC[!is.na(base$FC)]
median(fc)
quantile(fc, probs=c(0.25, 0.75))
sd(fc)

gcs <- base$GCS[!is.na(base$GCS)]
median(gcs)
quantile(gcs, probs=c(0.25, 0.75))
sd(gcs)

saps <- base$SAPS[!is.na(base$SAPS)]
median(saps)
quantile(saps, probs=c(0.25, 0.75))
sd(saps)

sofa <- base$SOFA_INIT[!is.na(base$SOFA_INIT)]
median(sofa)
quantile(sofa, probs=c(0.25, 0.75))
sd(sofa)

## 2- Distribution en fonction du bras de randomisation
tapply(base$AGE, base$TRAITEMENT, summary)
table(base$SEXE[base$TRAITEMENT == "A"])/242*100

quantile(base$AGE[base$TRAITEMENT == "A"], probs=c(0.25, 0.75))

hist_quantitative <- function(xx, name, unit, xxlim, yylim, seq) {
  x11()
  xx_safe <- xx[!is.na(xx)]
  xx_safe_median <- median(xx_safe)
  xx_safe_quantile <- quantile(xx_safe, probs=c(0.25, 0.75))
  xx_safe_sd <- sd(xx_safe)
  hist(xx_safe,
       col = grey(0.7),
       xlab = paste(name, unit),
       ylab = "Frequence",
       xlim = xxlim,
       ylim = yylim,
       main = paste("Histogramme ", name, sep = " "),
       breaks = seq)
  abline(v = xx_safe_median, col = "blue", lwd = 3, lty = 2)
  abline(v = xx_safe_quantile[1], col = "green", lwd = 3, lty = 2)
  abline(v = xx_safe_quantile[2], col = "red", lwd = 3, lty = 2)  
}

#breaks = seq(min(xx_safe)-xx_safe_sd, max(xx_safe)+xx_safe_sd)

## Histo AGE
hist_quantitative(base$AGE, "AGE", "(annees)",c(0, 100), c(0, 40), seq(10, 100, by=2))
legend(5, 35, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## Histo PH
hist_quantitative(base$PH, "PH", "", c(6.8, 7.8), c(0, 120), seq(6.8, 7.8, by=0.05))
legend(6.85, 100, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## Histo NA
hist_quantitative(base$NA., "NA", "(mmol/l)", c(100, 180), c(0, 80),seq(100, 180, by=1))
legend(160, 70, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## Histo TEMPERATURE
hist_quantitative(base$TEMPERATURE, "TEMPERATURE", "(°C)", c(28, 42), c(0, 70), seq(28, 42, by=0.5))
legend(28, 60, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## Histo K
hist_quantitative(base$K, "K", "(mmol/l)", c(0, 10), c(0, 80), seq(0, 10, by=0.2))
legend(7, 70, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## Histo BILIRUBINE
hist_quantitative(base$BILIRUBINE, "BILIRUBINE", "(mg/l)", c(0, 32), c(0, 160), seq(0, 32, by=0.5))
legend(20, 120, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## Histo UREE
hist_quantitative(base$UREE, "UREE", "(mmol/l)", c(0, 22), c(0, 80), seq(0, 22, 0.5))
legend(13, 70, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## HISTO HT
hist_quantitative(base$HT, "HT", "(%)", c(0, 60), c(0, 50), seq(0, 60, 1))
legend(2, 48, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## HISTO WBC
hist_quantitative(base$WBC, "WBC", "(10^3/mm^3)", c(0, 70), c(0, 50), seq(0, 70, 1))
legend(40, 45, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## HISTO PAM
hist_quantitative(base$PAM, "PAM", "(mmHg)", c(0, 300), c(0, 50), seq(0, 300, 5))
legend(180, 45, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## HISTO FC
hist_quantitative(base$FC, "FC", "(battements/min)", c(0, 180), c(0, 80), seq(0, 180, 5))
legend(7, 70, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## HISTO GCS
hist_quantitative(base$GCS, "GCS", "", c(0, 6), c(0, 250), seq(0, 6, 1))

## HISTO SAPS
hist_quantitative(base$SAPS, "SAPS", "", c(0, 100), c(0, 100), seq(0, 100, 5))
legend(70, 90, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## HISTO SOFA_INIT
hist_quantitative(base$SOFA_INIT, "SOFA_INIT", "", c(0, 30), c(0, 150), seq(0, 30, 2))
legend(18, 130, col = c("green","blue", "red"), c("1e quartile", "mediane", "3e quartile"), lty = c(2, 2))

## 4- Distribution de l'age en fonction du bras de randomisation
hist_quantitative(base$AGE[base$TRAITEMENT == "A"], "AGE", "(ans)",c(0, 100), c(0, 35), seq(10, 100, by=2))
hist_quantitative(base$AGE[base$TRAITEMENT == "B"], "AGE", "(ans)",c(0, 100), c(0, 35), seq(10, 100, by=2))
x11()
boxplot(base$AGE~base$TRAITEMENT,
        col = c("lightblue", "pink"),
        names=c("Traitement A", "Traitement B"),
        main = "Age en fonction du traitement",
        ylab = "Age (annees)")

## 5- Distribution de l'age en fonction d'une admission en urgence ou non
x11()
boxplot(base$AGE~base$ADMISSION_URGENT,
        col = c("lightblue", "pink"),
        names = c("Non admis en urgence", "Admis en urgence"),
        main = "Age en fonction d'une admission en urgence",
        ylab = "Age (années)")

## 6- Répresenter la PAM en fonction de la FC
x11()
plot(base$PAM~base$FC,
     main = "Pression artérielle moyenne en fonction de la fréquence cardiaque",
     xlab = "Fréquence cardiaque à l'inclusion (battements/min)",
     ylab = "Pression artérielle moyenne à l'inclusion (mmHg)",
     col = ifelse(base$SEXE== "M", "blue", "red"),
     pch = 19)
legend(40, 260, c("Homme", "Femme"), c("blue", "red"))

## 7- Représentation graphique de la distribution des départements d'origine
x11()
barplot(table(base$DEPARTEMENT),
        main = "Distribution des départements d'origine",
        xlab = "Département d'origine",
        ylab = "Frequence",
        col = c(grey(0.8), grey(0.7), grey(0.6), grey(0.5), grey(0.4), grey(0.3), grey(0.2), grey(0.1)))

barplot(table(base$SEXE),
        main = "Distribution des sexes",
        xlab = "Sexe",
        ylab = "Nombre",
        col = c("red", "blue"))

x11()
barplot(table(base$DEPARTEMENT[base$SEXE == "M"]),
        col = c("blue"),
        names = rep("", 8),
        space = c(1,3,3,3,3,3,3,3),
        ylim = c(0, 70),
        xlab = "Département",
        ylab = "Fréquence")
barplot(table(base$DEPARTEMENT[base$SEXE == "F"]),
        col = "pink",
        names = rep("", 8),
        space = c(2,3,3,3,3,3,3,3),
        add = T)
mtext("75", 1, line=0, at=2)
mtext("77", 1, line=0, at=6)
mtext("78", 1, line=0, at=10)
mtext("91", 1, line=0, at=14)
mtext("92", 1, line=0, at=18)
mtext("93", 1, line=0, at=22)
mtext("94", 1, line=0, at=26)
mtext("95", 1, line=0, at=30)
legend(9, 68,
       c("Homme", "Femme"),
       c("blue", "pink"))

## 8- Représentation graphique de SAPS en fonction de SOFA
x11()
plot(base$SAPS~base$SOFA_INIT,
     main = "Plot de SAPS en fonction du SOFA",
     col = grey(23:1/23),
     xlab = "SOFA_INIT",
     ylab = "SAPS")
x11()
boxplot(base$SAPS~base$SOFA_INIT,
        main = "Distribution de SAPS en fonction du SOFA",
        col = grey(23:1/23),
        xlab = "SOFA_INIT",
        ylab = "SAPS")
