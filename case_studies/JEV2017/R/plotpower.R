plotpower<-function(nsubj=20,
                    stddevquantiles=NULL,
                    posteriorquantiles=NULL,
                    posteriormeans=NULL,
                    mytitle=NULL){
sds<-seq(round(stddevquantiles[1]),
         round(stddevquantiles[2]),by=1)

powervals_upper<-rep(NA,length(sds))

for(i in 1:length(sds)){
  s<-sds[i]
  powervals_upper[i]<-power.t.test(d=posteriorquantiles[3],
                                   n=nsubj,
                                   sd=s,
                                   type="one.sample",
                                   alternative="two.sided")$power
}

powervals_mean<-rep(NA,length(sds))

for(i in 1:length(sds)){
  s<-sds[i]
  powervals_mean[i]<-power.t.test(d=posteriormeans,n=nsubj,sd=s,
                                  type="one.sample",
                                  alternative="two.sided")$power
}

powervals_lower<-rep(NA,length(sds))

for(i in 1:length(sds)){
  s<-sds[i]
  powervals_lower[i]<-power.t.test(d=posteriorquantiles[1],n=nsubj,sd=s,
                                   type="one.sample",
                                   alternative="two.sided")$power
}

#powerdistn<-data.frame(sds=sds,powervals_upper=powervals_upper,
#                       powervals_mean=powervals_mean,
#                       powervals_lower=powervals_lower)

plot(sds,powervals_upper,ylim=c(0,1),type="l",lty=1,
          xlab="standard deviations",
          ylab="power",
          main=paste(nsubj,"participants",sep=" "))
text(125,powervals_upper[which(sds==125)],as.character(posteriorquantiles[3]))


lines(sds,powervals_mean,lty=1)
text(125,powervals_mean[which(sds==125)],as.character(posteriormeans))

lines(sds,powervals_lower,lty=1)
text(125,powervals_lower[which(sds==125)],as.character(posteriorquantiles[1]))
}