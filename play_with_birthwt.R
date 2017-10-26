require(MASS)
data(birthwt)
View(birthwt)
?birthwt

motherWeightKg <- birthwt$lwt / 2.2
babyWeightKg <- birthwt$bwt / 1000

plot(birthwt$age,
     motherWeightKg,
     col = "red",
     pch = 19,
     main = "Mother and baby weight vs Mother age",
     xlab = "Age (years)", 
     ylab = "Weight (kg)",
     xlim = c(10, 50),
     ylim = c(0, 160))
points(birthwt$age[birthwt$smoke == 1],
       motherWeightKg[birthwt$smoke == 1],
       col = "blue",
       pch = 19)
points(birthwt$age,
       babyWeightKg,
       col = "green",
       pch = 19)
legend(x = 40,
       y = 160,
       legend = c("NS mum", "S mum", "baby"),
       col = c("red", "blue", "green"),
       pch = 19)
x11()

plot(birthwt$age,
     babyWeightKg,
     col = "red",
     pch = 19,
     main = "baby weight vs Mother age",
     xlab = "Age (years)", 
     ylab = "Weight (kg)",
     xlim = c(10, 50),
     ylim = c(0, 7))
legend(x = 45,
       y = 7,
       legend = c("Baby"),
       col = c("red"),
       pch = 19)
abline(h = 1, col = "blue", lwd = 3, lty = 2)
abline(h = 2, col = "green", lwd = 3, lty = 2)
abline(h = 3, col = "purple", lwd = 3, lty = 2)

X11()
boxplot(babyWeightKg~birthwt$smoke,
        col = c("red", "blue"),
        names = c("Non smoking", "Smoking"))
