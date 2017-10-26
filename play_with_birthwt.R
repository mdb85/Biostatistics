require(MASS)
data(birthwt)
View(birthwt)
#?birthwt  # Look for help

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

x11()
par(mfrow=c(2,1))
hist(birthwt$age[birthwt$smoke==0],
     col = grey(0.7),
     breaks = seq(10, 45, by=5))
hist(birthwt$age[birthwt$smoke==1],
     col = grey(0.7),
     breaks = seq(10, 45, by=5))

barplot(table(birthwt$smoke) / sum(table(birthwt$smoke)),
        main = "Smoking status",
        col = c("lightblue", grey(0.8)),
        name = c("Non smoking", "Smoking"))

pie(table(birthwt$race))

quants <- quantile(birthwt$age, probs=c(0.25, 0.50, 0.75), na.rm = T)
birthwt_c <-cut(birthwt$age, # Transform a quantitative variable into a qualitative variable
         c(0, 19, 23, 26, 50),
         include.lowest = TRUE)

X11()
boxplot(birthwt$bwt~birthwt_c,
        col = c("red", "blue", "grey", "purple"))

graphics.off()

boxplot(birthwt$bwt[birthwt$smoke == 0]~birthwt_c[birthwt$smoke == 0],
        col = "lightblue",
        at = c(1, 4, 7, 10),
        xlim = c(0, 12),
        names=rep("", 4))
boxplot(birthwt$bwt[birthwt$smoke == 1]~birthwt_c[birthwt$smoke == 1],
        col = "pink",
        add=T,
        at=c(2, 5, 8, 11),
        names=rep("", 4))
mtext("[14;19]", 1, line=2, at=1.5)
mtext("[19;23]", 1, line=2, at=4.5)
mtext("[23;26]", 1, line=2, at=7.5)
mtext("[26;50]", 1, line=2, at=10.5)
legend(1, 5000, 
       legend=c("Non smoking", "Smoking"),
       col=c("lightblue", "pink"),
       fill=c("lightblue", "pink"),
       horiz = T)