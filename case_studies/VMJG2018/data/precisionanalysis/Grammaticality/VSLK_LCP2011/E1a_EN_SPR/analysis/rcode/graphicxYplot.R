labmag <- 2

min <- min(verbrts$CI.lower,na.rm=TRUE)-50
max <- max(verbrts$CI.upper,na.rm=TRUE)+50

createPS("e1aenspr.ps")

print(xYplot(Cbind(M,CI.lower,CI.upper)~as.numeric(region),
             verbrts,groups=gram,
       layout=c(1,1),
       lty=1:2,
       type="b",
       lwd=3,
       cex=2,      
       pch=c(16,1),
       ylim=range(c(min,max)),
       label.curves=FALSE,
       xlab=list("Region",cex=labmag),
       scales = list(x = list(at=seq(1,4, by=1),alternating=1,
                       labels=c("V3","V2","V1","Post-V1"),
                       cex=labmag),  
       y=list(cex=labmag/1.5)),
       main=list("English SPR Expt. 1a",cex=labmag),
       ylab=list("Reading time [ms]",cex=labmag),
       ))

dev.off()
