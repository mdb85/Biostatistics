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
     ylim = c(0, 180))
points(birthwt$age[birthwt$smoke == 1],
       motherWeightKg[birthwt$smoke == 1],
       col = "blue",
       pch = 19)
points(birthwt$age,
       babyWeightKg,
       col = "green",
       pch = 19)
legend(x = 40,
       y = 180,
       legend = c("NS mum", "S mum", "baby"),
       col = c("red", "blue", "green"),
       pch = 19)
