y <- coefs.plot[,1]
lower <- coefs.plot[,2]
upper <- coefs.plot[,3]
min_y <- as.numeric(formatC(min(lower),digits=2))
max_y <- as.numeric(formatC(max(upper),digits=2))
offset=.05
p <- 3
cexmain <- 2
maintext <- "goobar"

##offset for x-axis

plot(c(1,2,3),y,axes=F,
     xlab ="",ylab="",
     pch=19,cex=1,col="gray",
     ylim=c(min_y,max_y+offset),
     main=maintext,
     cex.main=cexmain)

points(1:p,y,col="gray")

segments(c(1:p),lower,
         c(1:p),upper,
         lwd =  1.3,"black")

axis(1,c(1:p),labels=rep("",p),
     las=1,cex.axis=cexaxis)

axis(1,c(1:p),labels=xlab,tick=FALSE,
     las=1,cex.axis=cexmain)

axis(2,seq(min_y,max_y+0.05,by=.05),line=1,
     las=1,cex.axis=1.5)

abline(a=0,b=0,lty = 2,col=gray(0))
