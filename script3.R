require(titanic)
data(titanic_train)
dim(titanic_train) # Number of rows and columns
nrow(titanic_train) # Number of rows
ncol(titanic_train) # Number of columns
summary(titanic_train) # Gives a summary
str(titanic_train) # Describes the internal structure of an object
names(titanic_train) # Get the names of the variables
titanic_train$Sex <- as.factor(titanic_train$Sex) # Assign factors (like JAVA enumeration) to characters

# 1st step always describe the data, follow the rules:
 # Taille , Nb d'individu
 # Variable quantitative
   # Fréquence d'observation (35,2 %)  --> Toujours exclure les NA
     # [0-120] pas plus loing que que le %
     # > 120 , 1 chiffre après la virgule
     # > 10000, 2 chiffres après la virgule
   # % d'observation 
   # mean(standard deviation)
   # median[Q1; Q3]
   # Toujours mettre le nombre d'elements si y'en a qui manquent
 # Variable qualitative
   # 2 categories
     # ex:Sex(nbNA = 10)  F 314 (35,2 %) --> on ne donne qu'un seul
     # ex:Sex(n = 891)  F 314 (35,2 %) --> on ne donne qu'un seul
   # > 2 catégories
     # ex:  A  nA  (PA)
   # mean or median
     #ex: Age(n = 891 - 177)
     #ex: mean(titanic_train$Age, na.rm=T)
     #ex: median(titanic_train$Age)
   # Dispersion
     # Percentile:
       # quantile(titanic_train, probs=c(0.25, 0.75), na.rm=T)
     # variance:
       # var(titanic_train$Age)
     # Ecart type
       # sqrt(var(titanic_train$Age))
       # sd(titanic_train$Age)

barplot(table(titanic_train$Sex)) # used for qualitative variables
hist(titanic_train$Age, breaks=0:81) # used for quantitative variables, you can see bimodal representation
hist(titanic_train$Age, breaks=seq(0,80,by=10)) # We no longer see the bimodal representation => there's a compromise
hist(titanic_train$Age[titanic_train$Sex == "male"], breaks=0:81) 
boxplot(titanic_train$Age)
boxplot(titanic_train$Age~titanic_train$Sex)
