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

hist_quantitative <- function(xx, name, unit, xxlim, yylim, seq) {
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
hist_quantitative(base$AGE, "AGE", "(ans)",c(0, 100), c(0, 35), seq(10, 100, by=2))

## Histo PH
hist_quantitative(base$PH, "PH", "", c(6.8, 7.8), c(0, 120), seq(6.8, 7.8, by=0.05))

## Histo NA
hist_quantitative(base$NA., "NA", "(mmol/l)", c(100, 180), c(0, 80),seq(100, 180, by=0.1))

## Histo TEMPERATURE
hist_quantitative(base$TEMPERATURE, "TEMPERATURE", "(Â°C)", c(28, 42), c(0, 25), seq(28, 42, by=0.1))

## Histo K
hist_quantitative(base$K, "K", "(mmol/l)", c(0, 10), c(0, 40), seq(0, 10, by=0.1))

## Histo BILIRUBINE
hist_quantitative(base$BILIRUBINE, "BILIRUBINE", "(mg/l)", c(0, 32), c(0, 160), seq(0, 32, by=0.5))

## Histo UREE
hist_quantitative(base$UREE, "UREE", "(mmol/l)", c(0, 22), c(0, 80), seq(0, 22, 0.5))

## HISTO HT
hist_quantitative(base$HT, "HT", "(%)", c(0, 60), c(0, 50), seq(0, 60, 1))

## HISTO WBC
hist_quantitative(base$WBC, "WBC", "(10^3/mm^3)", c(0, 70), c(0, 50), seq(0, 70, 1))

## HISTO PAM
hist_quantitative(base$PAM, "PAM", "(mmHg)", c(0, 300), c(0, 50), seq(0, 300, 5))

## HISTO FC
hist_quantitative(base$FC, "FC", "(mmol/l)", c(0, 180), c(0, 80), seq(0, 180, 5))

## HISTO GCS
hist_quantitative(base$GCS, "GCS", "", c(0, 6), c(0, 250), seq(0, 6, 1))

## HISTO SAPS
hist_quantitative(base$SAPS, "SAPS", "", c(0, 100), c(0, 100), seq(0, 100, 5))

## HISTO SOFA_INIT
hist_quantitative(base$SOFA_INIT, "SOFA_INIT", "", c(0, 30), c(0, 150), seq(0, 30, 2))

## 4- Distribution de l'age en fonction du bras de randomisation
hist_quantitative(base$AGE[base$TRAITEMENT == "A"], "AGE", "(ans)",c(0, 100), c(0, 35), seq(10, 100, by=2))
hist_quantitative(base$AGE[base$TRAITEMENT == "B"], "AGE", "(ans)",c(0, 100), c(0, 35), seq(10, 100, by=2))
boxplot(base$AGE~base$TRAITEMENT,
        col = c("lightblue", "pink"),
        names=c("Traitement A", "Traitement B"),
        main = "Age en fonction du traitement")

## 5- Distribution de l'age en fonction d'une admission en urgence ou non
boxplot(base$AGE~base$ADMISSION_URGENT,
        col = c("lightblue", "pink"),
        names=c("Pas d'urgence", "Urgence"),
        main = "Age en fonction d'une admission en urgence")

## 6- Répresenter la PAM en fonction de la FC
FC_Homme <- base$FC[base$SEXE == "M"]
FC_Homme_sans_NA <- FC_Homme[!is.na(FC_Homme)]
age_Homme_sans_NA <- base$AGE[FC_Homme_sans_NA]
FC_Femme <- base$FC[base$SEXE == "F"]
FC_Femme_sans_NA <- FC_Femme[!is.na(FC_Femme)]
age_Femme_sans_NA <- base$AGE[FC_Femme_sans_NA]
plot(FC_Homme_sans_NA,
     age_Homme_sans_NA,
     col = "blue",
     pch = 19,
     main = "Age en fonction de la FC",
     xlab = "FC (battement/mn)", 
     ylab = "Age (ans)",
     xlim = c(10, 250),
     ylim = c(0, 160))
points(FC_Femme_sans_NA,
       age_Femme_sans_NA,
       col = "red",
       pch = 19)
legend(x = 170,
       y = 140,
       legend = c("Femme", "Homme"),
       col = c("red", "blue"),
       pch = 19)

## 7- Représentation graphique de la distribution des départements d'origine
barplot(table(base$DEPARTEMENT),
        main = "Distribution des départements d'origine",
        xlab = "Département",
        ylab = "Nombre")
barplot(table(base$SEXE),
        main = "Distribution des sexes",
        xlab = "Sexe",
        ylab = "Nombre",
        col = c("red", "blue"))
## 8- Représentation graphique de SAPS en fonction de SOFA
reduced_base <- base[,c("SAPS", "SOFA_INIT")]
reduced_base_without_NA <- na.omit(reduced_base) 
boxplot(reduced_base_without_NA$SAPS~reduced_base_without_NA$SOFA_INIT)
