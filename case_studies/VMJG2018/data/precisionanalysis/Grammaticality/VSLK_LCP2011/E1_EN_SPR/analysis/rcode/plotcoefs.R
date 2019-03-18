plotcoefs <- function(coefs.plot,
                      p=4,
                      offset=0.05,
                      maintext="Coefficients, with 95% HPD intervals",
                      cexmain=2,
                      cexaxis=1.3,
                      xlab=c("V3","V1","Post-V1")){


xlabrange <- c(1:(p-1),p-.05)

y <- coefs.plot[,1]
lower <- coefs.plot[,2]
upper <- coefs.plot[,3]
min_y <- as.numeric(formatC(min(lower),digits=2))
max_y <- as.numeric(formatC(max(upper),digits=2))

##offset for x-axis

plot(1:p,y,axes=F,xlab ="",ylab="",
     pch=19,cex=cexmain,col="gray",
     ylim=range(c(min_y,max_y)),
          main=maintext,
     cex.main=cexmain)

points(1:p,y,col="gray")

segments(c(1:p),lower,
         c(1:p),upper,
         lwd =  1.3,"black")

axis(1,c(1:p),labels=rep("",p),
     las=1,cex.axis=cexaxis)

axis(1,xlabrange,labels=xlab,tick=FALSE,
     las=1,cex.axis=cexmain)

axis(2,seq(min_y,max_y+0.05,by=.05),line=1,
     las=1,cex.axis=1.5)

abline(a=0,b=0,lty = 2,col=gray(0))

}

