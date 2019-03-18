## This file contains the non-public version of the data analyses for
##  Expt 1a presented in the paper:
#   Shravan Vasishth, Katja Suckow, Richard Lewis, and Sabine Kern.
#   Short-term forgetting in sentence comprehension:
#   Crosslinguistic evidence from head-final structures.
#   Submitted to Language and Cognitive Processes, 2007.

library(lme4)
library(car)
#library(coda)
library(Hmisc)

## analyzing question-responses using logistic regression
## question responses
alldataq <- read.table("../../data/allquestions.txt")
colnames(alldataq) <- c("subject","expt","item","condition","question","response","correct","qrt")

## remove practice items
alldataq <- subset(alldataq,expt!="practice")

## mean and se of all responses:
mean(with(alldataq,tapply(correct,subject,mean)))
se(with(alldataq,tapply(correct,subject,mean)))

alldataq.gug <- subset(alldataq,expt=="gug")
alldataq.gug$condition <- factor(alldataq.gug$condition)


## code up grammaticality factor:
alldataq.gug$gram <- ifelse(alldataq.gug$condition%in%c("a","b"),1,-1)

## code up interference factor:
alldataq.gug$int <- ifelse(alldataq.gug$condition%in%c("a","c"),1,-1)

## means and se of engug expt, gram:
with(alldataq.gug,tapply(correct,gram,mean,na.rm=TRUE))*100
with(alldataq.gug,tapply(correct,gram,se))*100

## means and se of engug expt, int:
with(alldataq.gug,tapply(correct,int,mean,na.rm=TRUE))*100
with(alldataq.gug,tapply(correct,int,se))*100



means.subj.gug <- (with(alldataq.gug,tapply(correct,IND=list(subject,gram),mean,na.rm=TRUE))*100)
barplot(means.subj.gug[,2]-means.subj.gug[,1])

summary(lmer(correct~gram*int+(1|subject)+(1|item),family=binomial(),alldataq.gug))
=
## old model's interpretation (not relevant any more):
## Intercept: if the sentence is grammatical (gram==0), the odds of
## giving a correct answer is not significantly higher than 50% (chance):
## exp(0.1650)=1.179393
## p_correct=1.179393/(1+1.179393)=0.5411566

## slope:
## the probability of giving a correct answer in the ungrammatical case is
## exp(0.3913)/(1+exp(0.3913))=0.5965956
## and this is significantly above chance

## Process data for analysis:
source("processdata.R",echo=T)

#save(verbrts,file="verbrts.Rda")
#save(critdata,file="critdata.Rda")

#gram <- ifelse(critdata$condition%in%c("a","b"),-1,1)
#int <- ifelse(critdata$condition%in%c("a","c"),-1,1)

#critdata$gram <- gram
#critdata$int <- int

#critdata2 <- subset(critdata,value<3000)

## main graphic with matplot
gram.means <- subset(verbrts.g,gram=="gram")
ungram.means <- subset(verbrts.g,gram=="ungram")

graycol <- gray(.5)

filename <- "e1aensprplotgram.ps" 

createPS(filename)

matplot(gram.means$M,
        ylim=c(min(verbrts.g$CI.lower,na.rm=T),
               max(verbrts.g$CI.upper,na.rm=T)),
        main="Experiment 1a (English SPR)",
        type="b",
        pch=16,
        lty=1,
        xaxt="n",
        cex.main=1.8,
        lwd=5,
        ylab="",
        cex.axis=1.8)

lines(ungram.means$M,lwd=3,lty=2,col=graycol)
points(ungram.means$M,pch=19,cex=1.5,col=graycol)

arrows(seq(from=1,4,by=1),gram.means$CI.lower,
       seq(from=1,4,by=1),gram.means$CI.upper,
       lwd =  2,col="black",angle=90,code=3,length=.1)

arrows(seq(from=1,4,by=1),ungram.means$CI.lower,
       seq(from=1,4,by=1),ungram.means$CI.upper,
       lwd =  2,col=graycol,angle=90,code=3,length=.1)

mtext(text=c("V3","V2","V1","Post-V1"),
      side=1,line=.75,at=1:4,cex=2)
mtext(text="Reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)

legend(2.4,1100,lty=c(1,2),col=c("black",graycol),pch=c(16,19),
       legend=c("grammatical","ungrammatical"),cex=2,lwd=3)

dev.off()

destination <- "~/Dropbox/Papers/IntML/Figures"
system(paste("cp ",filename,destination,sep=" "))

## interference:

hi.means <- subset(verbrts.i,int=="hi")
lo.means <- subset(verbrts.i,int=="lo")

filename <- "e1aensprplotint.ps"
createPS(filename)

matplot(hi.means$M,
        ylim=c(min(verbrts.i$CI.lower,na.rm=T),
               max(verbrts.i$CI.upper,na.rm=T)),
        main="Experiment 1a (English SPR): interference",
        type="b",
        pch=16,
        lty=1,
        xaxt="n",
        cex.main=1.8,
        lwd=5,
        ylab="",
        cex.axis=1.8)

lines(lo.means$M,lwd=3,lty=2,col=graycol)
points(lo.means$M,pch=19,cex=1.5,col=graycol)

arrows(seq(from=1,5,by=1),hi.means$CI.lower,
       seq(from=1,5,by=1),hi.means$CI.upper,
       lwd =  2,col="black",angle=90,code=3,length=.1)

arrows(seq(from=1,5,by=1),lo.means$CI.lower,
       seq(from=1,5,by=1),lo.means$CI.upper,
       lwd =  2,col=graycol,angle=90,code=3,length=.1)

mtext(text=c("NP3","V3","V2","V1","Post-V1"),side=1,line=.75,at=c(1,2,3,4,4.9),cex=2)
mtext(text="Reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)

legend(2.4,1260,lty=c(1,2),col=c("black",graycol),pch=c(16,19),
       legend=c("high interference","low interference"),
       cex=2,lwd=3)

dev.off()

system(paste("cp ",filename,destination,sep=" "))

## orthogonal coding:
g <- ifelse(critdata$condition%in%c("a","b"),1,-1)
i <- ifelse(critdata$condition%in%c("a","c"),1,-1)
gxi <- ifelse(critdata$condition%in%c("a","d"),-1,1)

critdata$g <- g
critdata$i <- i
critdata$gxi <- gxi

## comparison 0 at np3:
data0 <- subset(critdata,region=="NP3")

summary(fm0 <- lmer(log(value)~ g + i + gxi + (1|subject)+(1|item),
                   data=data0))

qq.plot(residuals(fm0))

print(dotplot(ranef(fm0, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subject)

print(dotplot(ranef(fm0, post = TRUE),
              strip=FALSE,main="Predictions for Items Random Effects")$item)

## comparison 1 at v3:
data1 <- subset(critdata,region=="V3")

summary(fm1 <- lmer(log(value)~ g + i + gxi + (1|subject)+(1|item),
                   data=subset(data1,value<2000)))

qq.plot(residuals(fm1))

print(dotplot(ranef(fm1, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subject)

print(dotplot(ranef(fm1, post = TRUE),
              strip=FALSE,main="Predictions for Items Random Effects")$item)

## comparison 2 V2 in gram and V1 in ungrammatical:

data2 <- subset(critdata,(region=="V2" & g==1) |
                                        (region=="V1" & g==-1))

## since word length differs in critical regions, add that as a covariate
data2$wl <- nchar(as.character(data2$word))

## compare word lengths
with(data2,tapply(wl,gram,mean))
with(data2,tapply(wl,gram,se))

summary(fm2 <- lmer(log(value)~ g+i+gxi+center(wl)+(1|subject)+(1|item),
                   data=data2))

qq.plot(residuals(fm2))

print(dotplot(ranef(fm2, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subject)

print(dotplot(ranef(fm2, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$item)


## comparison 3 V1:
data3 <- subset(critdata,(region=="V1"))


summary(fm3 <- lmer(log(value)~ g+i+gxi+(1|subject)+(1|item),
                   data=subset(data3,value<2000)))

summary(fm3 <- lmer(value~ g+i+gxi+(1|subject)+(1|item),
                   data=subset(data3,value<2000)))



summary(fm3 <- lmer(log(value)~ g+i+gxi+(1|subject)+(1|item),
                   data=data3))

qq.plot(residuals(fm3))

print(dotplot(ranef(fm3, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subject)

print(dotplot(ranef(fm3, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$item)

## comparison 4 post V1 region:
data4 <- subset(critdata,(region=="postV1"))

summary(fm4 <- lmer(log(value)~ g+i+gxi+(1|subject)+(1|item),
                   data=subset(data4,value<2000)))

summary(fm4 <- lmer(value~ g+i+gxi+(1|subject)+(1|item),
                   data=subset(data4,value<2000)))

summary(fm4 <- lmer(log(value)~ g+i+gxi+(1|subject)+(1|item),
                   data=subset(data4,value<2000)))

qq.plot(residuals(fm4))

print(dotplot(ranef(fm4, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subject)

print(dotplot(ranef(fm4, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$item)

## save data for meta-analysis
#write(t(critdata),ncolumns=10,file="e1ensprcrit.txt")

mc.fm0 <- mcmcsamp(fm0,50000)
mc.fm1 <- mcmcsamp(fm1,50000)
mc.fm2 <- mcmcsamp(fm2,50000)
mc.fm3 <- mcmcsamp(fm3,50000)
mc.fm4 <- mcmcsamp(fm4,50000)

hpd.fm0 <- lmerHPD(mc.fm0)
hpd.fm1 <- lmerHPD(mc.fm1)
hpd.fm2 <- lmerHPD(mc.fm2)
hpd.fm3 <- lmerHPD(mc.fm3)
hpd.fm4 <- lmerHPD(mc.fm4)

coef.fm0 <- fixef(fm0)
coef.fm1 <- fixef(fm1)
coef.fm2 <- fixef(fm2)[1:4]
coef.fm3 <- fixef(fm3)
coef.fm4 <- fixef(fm4)

coefs <- c(coef.fm0,
           coef.fm1,
           #coef.fm2,
           coef.fm3,coef.fm4)

hpds <- rbind(hpd.fm0$fixef[1:4,],
              hpd.fm1$fixef[1:4,],
#              hpd.fm2$fixef[1:4,], ## no need to plot this
              hpd.fm3$fixef[1:4,],
              hpd.fm4$fixef[1:4,])

g.hpd <- hpds[c(2,6,10,14),]
i.hpd <- hpds[c(3,7,11,15),]

g.coefs.plot <- cbind(coefs[c(2,6,10,14)],g.hpd)
i.coefs.plot <- cbind(coefs[c(3,7,11,15)],i.hpd)

library(xtable)

xtable(coefs.plot,digits=5)

source("plotcoefs.R")

createPS("e1aensprHPD.ps")

plotcoefs(g.coefs.plot,p=4,
          xlab=c("NP3","V3","V1","Post-V1"),
          maintext="Experiment 1a: grammaticality effect (HPD intervals)")

dev.off()

round(hpd.fm0$fixef,digits=5)
round(hpd.fm1$fixef,digits=5)
round(hpd.fm3$fixef,digits=5)
round(hpd.fm4$fixef,digits=5)

##
### save for meta analysis:
## grammaticality factor:
#critdata$gram <- as.factor(ifelse(critdata$condition%in%c("a","b"),"gram","ungram"))

## interference factor:
#critdata$int <- as.factor(ifelse(critdata$condition%in%c("a","c"),"hi","lo"))

critdata$expt <- "e1a"
critdata$lang <- "english"
critdata$method <- "spr"
write.table(critdata,file="e1acritdata.txt")
