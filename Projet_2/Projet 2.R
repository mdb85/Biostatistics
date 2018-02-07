## PROJET 2 : BIOSTATISTIQUES

getwd()
BP2<-read.csv2("base_projet2.csv")
BP2

## QUESTION 1 : Le traitement a t il un effet sur la mortalité à J28 ? 
#                Si oui lequel ? 
#                Quelle type d'erreur statistique avez vous pu commettre?

# COMPARAISON D'UNE PROPORTION A UNE VALEUR THEORIQUE : TEST DE L'ECART-REDUIT
# CA : N x PI0 > 5 et N x (1 - PI0) > 5

summary(BP2)
N<-length(BP2$PATIENT_NUM)
PI0=0.5

N*PI0 # CA VALIDEE
N*(1-PI0) # CA VALIDEE

P<-table(BP2$DECES==1)/500
N<-length(BP2$PATIENT_NUM)
PI0=0.5

Z<-abs((0.108-PI0)/sqrt((PI0*(1-PI0)/N)))
Z

EPSILON<-qnorm(0.975)

# REJET DE H0 CAR Z > EPSILON : TRAITEMENT => MOINS DE DECES
# ON A PRIS LE RISQUE DE REJETER H0 ALORS QUE H0 EST VRAIE (ERREUR DE TYPE 1)

## QUESTION 2 : Le SOFA à J1 est-il diérent du SOFA initial ?
#                Donner l'estimation de l'évolution moyenne du SOFA et son intervalle de confiance à 95%?

# t.test(BP2$SOFA_INIT,BP2$SOFA_J1,paired=T)

#        Paired t-test

# data:  BP2$SOFA_INIT and BP2$SOFA_J1
# t = 3.9528, df = 475, p-value = 8.898e-05
# rejet de H0 => différence dans les SOFA
# 95 percent confidence interval: [0.4416182 ; 1.3146843]
# mean of the differences : 0.8781513 

## QUESTION 3 : On cherche maintenant les facteurs pronostiques du décès en réanimation. 
#                Parmi les facteurs suivant lesquels sont pronostiques du décès en réanimation à J28.

fisher.test(matrix(c(1,9,11,3),nrow=2))

# SEXE

table(BP2$SEXE[BP2$DECES==1])
fisher.test(matrix(c(26,288,28,158),ncol=2),y=NULL)
chisq.test(matrix(c(26,288,28,158),ncol=2),y=NULL)
#Rejet de H0 => Lien entre Sexe et Décès

# AGE A L'INCLUSION

table(BP2$DECES[BP2$AGE>63])
fisher.test(matrix(c(16,223,38,223),ncol=2),y=NULL)
chisq.test(matrix(c(16,223,38,223),ncol=2),y=NULL)
#Rejet de H0 => Lien entre Age et Décès


# AGE A L'INCLUSION SUPERIEUR A 70

table(BP2$DECES[BP2$AGE<=70])
fisher.test(matrix(c(23,290,31,156),ncol=2),y=NULL)
chisq.test(matrix(c(23,290,31,156),ncol=2),y=NULL)
#Rejet de H0 => Lien entre Age et Décès

# SIDA A L'INCLUSION

table(BP2$DECES[BP2$SIDA==1])
fisher.test(matrix(c(54,443,0,3),ncol=2),y=NULL)
#Non rejet de H0 => Pas de lien entre SIDA et Décès

# INSUFFISANCE CARDIAQUE A L'INCLUSION

table(BP2$DECES[BP2$INSUF_CARD==1])
fisher.test(matrix(c(17,87,37,359),ncol=2),y=NULL)
#Rejet de H0 => Lien entre Insuffisance cardiaque et Décès

# ATTEINTE HEPATIQUE A L'INCLUSION

table(BP2$DECES[BP2$ATTEINTE_HEPATIQUE==0])
fisher.test(matrix(c(6,11,48,435),ncol=2),y=NULL)
#Rejet de H0 => Lien entre Atteinte hépatique et Décès

# Tumeur à l'inclusion

table(BP2$DECES[BP2$TUMEUR==1])
fisher.test(matrix(c(16,55,38,391),ncol=2),y=NULL)

# CANCER METASTATIQUE A L'INCLUSION

table(BP2$DECES[BP2$CANCER_METASTATISTIQUE==1])
fisher.test(matrix(c(6,15,48,431),ncol=2),y=NULL)
#Rejet de H0 => Lien entre Cancer métastatique et Décès

# TEMPERATURE CORPORELLE

summary(BP2$TEMPERATURE)
table(BP2$DECES[BP2$TEMPERATURE<=37])
fisher.test(matrix(c(23,82,17,97),ncol=2),y=NULL)
#Non rejet de H0 => Pas de lien entre Température et Décès

# PRESSION ARTERIELLE MOYENNE A L'INCLUSION

summary(BP2$PAM)
table(BP2$DECES[BP2$PAM<=84])
fisher.test(matrix(c(23,147,17,100),ncol=2),y=NULL)
#Non rejet de H0 => Pas de lien entre PAM et Décès

# FREQUENCE CARDIAQUE A L'INCLUSION

summary(BP2$FC)
table(BP2$DECES[BP2$FC<=88])
fisher.test(matrix(c(22,242,30,188),ncol=2),y=NULL)
#Non rejet de H0 => Pas de lien entre FC et Décès

# SAPS A L'INCLUSION

summary(BP2$SAPS)
table(BP2$DECES[BP2$SAPS<=40])
fisher.test(matrix(c(5,248,49,198),ncol=2),y=NULL)
#Rejet de H0 => Lien entre SAPS et Décès

# SOFA A L'INCLUSION

summary(BP2$SOFA_INIT)
table(BP2$DECES[BP2$SOFA_INIT>6])
fisher.test(matrix(c(15,251,36,174),ncol=2),y=NULL)
#Rejet de H0 => Lien entre SOFA_INIT et Décès

########## Pour Chi2 : Si pvalue > 0,05 -> Non rejet de H0 ##########

## QUESTION 4 : Indiquer la valeur du risque attribuable, du risque relatif et de l'Odds Ratio du sexe féminin par rapport
#                au sexe masculin sur la mortalité à J28.

# RISQUE RELATIF
a <- 28
c <- 158
b <- 26
d <- 288
or <- (a*d)/(b*c)
rr <- (a/(a+c))/(b/(b+d))
# OR = 1.962
# RR = 1.818

## QUESTION 5: courbe ROC
# package pROC
require(pROC)
result <- roc(BP2$DECES, BP2$SAPS)
plot(result)
plot(roc(BP2$DECES, BP2$SAPS))
result$thresholds[result$sensitivities == 1]
result$specificities[result$sensitivities == 1]
seuil <- coords(result, "best", ret = "threshold")
se_seuil <- result$sensitivities[result$thresholds == 36.5]
sp_seuil <- result$specificities[result$thresholds == 36.5]
# seuil =36.5
# se_seuil = 0.962963
# sp_seuil = 0.5134529

##QUESTION 6: 

# SEXE

table(BP2$SEXE[BP2$DECES==1 & BP2$AGE>70])
fisher.test(matrix(c(13,94,18,62),ncol=2),y=NULL)
#Rejet de H0 => Lien entre Sexe et Décès

# AGE 
fisher.test(matrix(c(12,85,19,71),ncol=2),y=NULL)

# SIDA
fisher.test(matrix(c(0,0,31,151),ncol=2),y=NULL)

# IC
fisher.test(matrix(c(12,48,19,108),ncol=2),y=NULL)

# Atteinte hépatique
fisher.test(matrix(c(2,0,29,156),ncol=2),y=NULL)

# Tumeur
fisher.test(matrix(c(10,25,21,131),ncol=2),y=NULL)

# Cancer
fisher.test(matrix(c(3,5,28,151),ncol=2),y=NULL)

# Temperature
fisher.test(matrix(c(14,39,7,31),ncol=2),y=NULL)

# PAM
fisher.test(matrix(c(12,53,13,38),ncol=2),y=NULL)

# FC
fisher.test(matrix(c(15,81,15,68),ncol=2),y=NULL)

# SAPS
fisher.test(matrix(c(8,92,23,64),ncol=2),y=NULL)

# SOFA
fisher.test(matrix(c(12,83,18,68),ncol=2),y=NULL)

##QUESTION 7:
length(BP2$DECES[BP2$AGE <= 70])
table(BP2$DECES[BP2$AGE <= 70])
length(BP2$DECES[BP2$AGE > 70])
table(BP2$DECES[BP2$AGE > 70])
table(BP2$TRAITEMENT[BP2$AGE <= 70])

fisher.test(matrix(c(12,136,11,154),ncol=2),y=NULL)
fisher.test(matrix(c(22,72,9,84),ncol=2),y=NULL)
