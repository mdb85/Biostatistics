xx <- 1:12
yy <- rep(3:1, 4)
plot(xx, yy)
plot(yy~xx) #plot yy 
rep(1:3, each=4) #repeat each element 4 times

x11() #open a new window
zz<-1:12
plot(xx,zz)
dev.copy2pdf(file="C:\Users\169\Desktop\fig1.pdf")

graphics.off()

x11(width=4, height=8)
plot(xx, zz)

plot(xx, zz, col="red")
plot(xx, zz, col="blue")
plot(xx, zz, col=rep(c("red", "blue"), 6), pch=19, cex=3)
plot(xx, zz, col=1:12, pch=19, cex=3)
plot(1:25, 1:25, col=1:25, pch=1:25, cex=3)
plot(1:25, 1:25, col=1:25, pch="a", cex=3) #plot a at each point
plot(xx, zz, col=1:12, pch=19, cex=3, type="l", lwd=6) # plot lines
plot(xx, zz, col=1:12, pch=19, cex=3, type="l", lwd=6, lty=2) # lty --> pointillés
plot(xx, yy, col=1:12, pch=19, cex=3, type="b", lwd=6) # type "b" --> both line and point
plot(xx, yy, col=1:12, pch=19, cex=3, type="b", lwd=6,
     main="Title", xlab="X axis", ylab="Y axis") # Define a title and X and y axis

velo <- read.delim("C:/Users/169/Desktop/velo.txt", dec=",")
plot(velo$psaavt, velo$psaavl, col="red", pch=19, main="PSA libres vs PSA totaux", xlab="Dosage PSA totaux", ylab="Dosage PSA libres",
     xlim=c(0,25), ylim=c(0,3))
points(velo$psaavt[velo$Poids < 70], velo$psaavl[velo$Poids < 70], col="pink", pch=19) # Plots on a an existing plot
points(velo$psaapt, velo$psaapl, col="blue", pch=19) # Plots on a an existing plot
legend(15, 2, legend=c("PSA Av", "PSA ap"), 
       col=c("red", "blue"), pch=19)
abline(h=2, col="green", lwd=2) #Adds a horizontal line

