base <- read.csv2("/home/mdb/Sources/Biostatistics/base_projet1.csv")
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

hist_quantitative <- function(xx, name) {
  xx_safe <- xx[!is.na(xx)]
  xx_safe_median <- median(xx_safe)
  xx_safe_quantile <- quantile(xx_safe, probs=c(0.25, 0.75))
  xx_safe_sd <- sd(xx_safe)
  hist(xx_safe,
       col = grey(0.7),
       xlab = name,
       ylab = "Nombre",
       main = paste("Histogramme ", name, sep = " "),
       breaks = seq(min(xx_safe)-10, max(xx_safe)+10, by=5))
  abline(v = xx_safe_median, col = "blue", lwd = 3, lty = 2)
  abline(v = xx_safe_quantile[1], col = "green", lwd = 3, lty = 2)
  abline(v = xx_safe_quantile[2], col = "red", lwd = 3, lty = 2)  
}

hist_quantitative(base$AGE, "AGE")

## Histo AGE
age <- base$AGE[!is.na(base$AGE)]
age_m <- median(age)
age_quantile <- quantile(age, probs=c(0.25, 0.75))
sd(age)
hist(age,
     col = grey(0.7),
     xlab = "AGE",
     ylab = "Nombre",
     main = "Distribution de l'age",
     breaks = seq(0, 100, by=5))
abline(v = age_m, col = "blue", lwd = 3, lty = 2)
abline(v = age_quantile[1], col = "green", lwd = 3, lty = 2)
abline(v = age_quantile[2], col = "red", lwd = 3, lty = 2)

## Histo PH
ph <- base$PH[!is.na(base$PH)]
ph_m <- median(ph)
ph_quantile <- quantile(ph, probs=c(0.25, 0.75))
sd(ph)
hist(ph,
     col = grey(0.7),
     xlab = "PH",
     ylab = "Nombre",
     main = "Distribution du PH",
     breaks = seq(0, 100, by=5))
abline(v = ph_m, col = "blue", lwd = 3, lty = 2)
abline(v = ph_quantile[1], col = "green", lwd = 3, lty = 2)
abline(v = ph_quantile[2], col = "red", lwd = 3, lty = 2)

## Histo NA
na <- base$NA.[!is.na(base$NA.)]
na_m <- median(na)
na_quantile <- quantile(na, probs=c(0.25, 0.75))
sd(na)
