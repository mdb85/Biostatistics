velo <- read.delim("C:/Users/169/Desktop/velo.txt", dec=",")
velo$BMI[5]
velo[5, "BMI"]
velo[5,] # Retrieve all the line
velo[,5] # Retrieve the column
velo[1:5,] # Retrieve the 5 first rows
bb<-c(1,10,100)
velo[bb,] # Retrieve row 1, row 10, and row 100
velo[,3] / velo[,4]^2 # Compute BMI
velo$BMI2 <- velo$Poids/velo$Taille^2 # Compute the BMI and store it in BMI2
View(velo) # Show the velo table
typeof(velo) # show the type of velo
velo$test <- (velo$BMI - velo$BMI2) < 0.0000001
"table(velo$test)",]
which((velo$BMI - velo$BMI2) > 0.0000001) # which values are different
velo[which((velo$BMI - velo$BMI2) > 0.0000001),]$BMI <- velo[which((velo$BMI - velo$BMI2) > 0.0000001),]$BMI2
velo2 <- velo[velo$Opere=="Oui",] # selec patient that had surgery
velo2 <- velo[velo$Opere=="oui" & velo$Age > 50,] # Multiple conditions using &
velo2 <- velo[velo$Opere=="oui" | velo$Age > 50,] # Multiple conditions using |
mean(velo$Age) # Compute the mean
median(velo$Age, T) # Compute the median
??median # Look up the median function (helper)

