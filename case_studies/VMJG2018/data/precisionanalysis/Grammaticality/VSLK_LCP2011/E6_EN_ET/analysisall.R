## This file contains the non-public version of the data analyses for
##  Expt 7 presented in the paper:
#   Shravan Vasishth, Katja Suckow, Richard Lewis, and Sabine Kern.
#   Short-term forgetting in sentence comprehension:
#   Crosslinguistic evidence from head-final structures.
#   Submitted to Language and Cognitive Processes, 2007.

library(lme4)
library(car)
library(Hmisc)

## analyzing question-responses using logistic regression
## question responses
alldata <- read.table("Data/expt6data.txt")

#subset(alldata,subject==101 & run=="first" & comma=="nocomma")
#subset(alldata,subject==101 & run=="first" & comma=="comma")

colnames(alldata) <- c("run","comma","subject","subj","expt","item","condition","question","response","correct","rt")

alldata$subject <- factor(paste(alldata$subject,alldata$comma,sep=""))

## remove practice items
alldata <- subset(alldata,expt!="practice")

alldataq<-subset(alldata,question=="?")

summary(alldataq)

alldataq$correct <- as.numeric(as.character(alldataq$correct))

summary(alldataq)

## mean and se of all responses:
mean(with(alldataq,tapply(correct,subject,mean)))
se(with(alldataq,tapply(correct,subject,mean)))

alldataq.gug <- subset(alldataq,expt=="gug")
alldataq.gug$condition <- factor(alldataq.gug$condition)

## code up grammaticality factor:
alldataq.gug$gram <- ifelse(alldataq.gug$condition%in%c("a","c"),-1,1)

## means and se of engug expt, gram:
with(alldataq.gug,tapply(correct,gram,mean,na.rm=TRUE))*100
with(alldataq.gug,tapply(correct,gram,se))*100

means.subj.gug <- (with(alldataq.gug,tapply(correct,IND=list(subject,gram),mean,na.rm=TRUE))*100)
barplot(means.subj.gug[,2]-means.subj.gug[,1])

summary(lmer(correct~gram+comma+(1|subject)+(1|item),family=binomial(),alldataq.gug))

## gram: pos 6,7,8,9
## ungram 6 7 8

## reading time analysis:
data <- subset(alldata,question!="?")

summary(data)

colnames(data) <- c("run","comma","subj","subject","expt","item","condition","pos","word","dummy2","rt")

data$pos <- as.numeric(as.character(data$pos))

data.gug <- subset(data,expt=="gug")

summary(data.gug)

data.gug$expt <- factor(data.gug$expt)

data.gug$condition <- factor(data.gug$condition)

data.gug$pos <- data.gug$pos+1

gram <- subset(data.gug,pos%in%c(6,7,8,9))

summary(gram$rt)
boxplot(gram$rt)
sd(gram$rt)*3

gram <- subset(gram,rt<2000)

boxplot(log(gram$rt))

crit <- with(gram,tapply(rt,IND=list(condition,pos),mean))

crit.ci.a <- with(subset(gram,condition=="a"),tapply(rt,IND=list(pos),ci))
crit.ci.b <- with(subset(gram,condition=="b"),tapply(rt,IND=list(pos),ci))
crit.ci.c <- with(subset(gram,condition=="c"),tapply(rt,IND=list(pos),ci))
crit.ci.d <- with(subset(gram,condition=="d"),tapply(rt,IND=list(pos),ci))


ci.lower.a <- c(crit.ci.a$`6`$lower,
                crit.ci.a$`7`$lower,
                crit.ci.a$`8`$lower,
                crit.ci.a$`9`$lower)

ci.upper.a <- c(crit.ci.a$`6`$upper,
                crit.ci.a$`7`$upper,
                crit.ci.a$`8`$upper,
                crit.ci.a$`9`$upper)

ci.lower.b <- c(crit.ci.b$`6`$lower,
                               NA,
                crit.ci.b$`7`$lower,
                crit.ci.b$`8`$lower)

ci.upper.b <- c(crit.ci.b$`6`$upper,
                NA,                
                crit.ci.b$`7`$upper,
                crit.ci.b$`8`$upper)

ci.lower.c <- c(crit.ci.c$`6`$lower,
                crit.ci.c$`7`$lower,
                crit.ci.c$`8`$lower,
                crit.ci.c$`9`$lower)

ci.upper.c <- c(crit.ci.c$`6`$upper,
                crit.ci.c$`7`$upper,
                crit.ci.c$`8`$upper,
                crit.ci.c$`9`$upper)

ci.lower.d <- c(crit.ci.d$`6`$lower,
                               NA,
                crit.ci.d$`7`$lower,
                crit.ci.d$`8`$lower)

ci.upper.d <- c(crit.ci.d$`6`$upper,
                NA,                
                crit.ci.d$`7`$upper,
                crit.ci.d$`8`$upper)



b.V1 <- crit[2,2]
b.postV1 <- crit[2,3]

d.V1 <- crit[4,2]
d.postV1 <- crit[4,3]

crit[2,2] <- NA
crit[4,2] <- NA

crit[2,3] <- b.V1
crit[2,4] <- b.postV1

crit[4,3] <- d.V1
crit[4,4] <- d.postV1


barplot(crit,beside=T)

## code up grammaticality:
g <- ifelse(gram$condition%in%c("a","c"),-1,1)

gram$g <- g

length(unique(subset(gram,comma=="comma")$subj)) ## 25
length(unique(subset(gram,comma=="nocomma")$subj)) ## 25


## V3
(fm.v3 <- lmer(log(rt)~g+comma+(1|subj)+(1|item),subset(gram,pos==6 & rt<2000)))

(fm.v3 <- lmer(log(rt)~condition+(1|subj)+(1|item),subset(gram,pos==6)))

qq.plot(resid(fm.v3))

## V1
(fm.v1 <- lmer(log(rt)~g+(1|subj)+(1|item),subset(gram,(condition%in%c("a","c") & pos==8) | (condition%in%c("b","d") & pos==7))))

(fm.v1 <- lmer(log(rt)~g*comma+(1|subj)+(1|item),subset(gram,(condition%in%c("a","c") & pos==8) | (condition%in%c("b","d") & pos==7) & rt<2000)))

qq.plot(resid(fm.v1))

## post-V1
(fm.postv1 <- lmer(log(rt)~g+(1|subj)+(1|item),subset(gram,comma=="comma" & (condition%in%c("a","c") & pos==9) | (condition%in%c("b","d") & pos==8)  & rt<2000)))

qq.plot(resid(fm.postv1))

## mcmc
mc.v1     <- mcmcsamp(fm.v1,50000)
mc.postv1 <- mcmcsamp(fm.postv1,50000)

hpd.modelv1 <- lmerHPD(mc.v1)
hpd.modelpostv1 <- lmerHPD(mc.postv1)


## graphics:


createPS("engugexpt6.ps")
## with lineplots:

ab.means <- crit

multiplot(1,2)

matplot(ab.means[1,],
        ylim=c(min(ci.lower.c,na.rm=T)-50,max(ci.upper.c,na.rm=T)+50),
        type="b",pch=16,xaxt="n",
        main="Exp. 6a (commas)",cex.main=2,lwd=3,ylab="",
        cex.axis=1.8)

lines(ab.means[2,],lwd=3,col="black",lty=2)
points(ab.means[2,],pch=19,col="black")

arrows(seq(from=1,4,by=1),ci.lower.a,
       seq(from=1,4,by=1),ci.upper.a,
       lwd=2,col="black",angle=90,code=3,length=.1)

arrows(seq(from=1,4,by=1),ci.lower.b,
       seq(from=1,4,by=1),ci.upper.b,
       lwd =  2,col="black",angle=90,code=3,length=.1)

mtext(text=c("V3","V2","V1","Post-V1"),side=1,line=1.5,at=1:4,cex=2)
mtext(text="Reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)

matplot(ab.means[3,],
        ylim=c(min(ci.lower.c,na.rm=T)-50,max(ci.upper.c,na.rm=T)+50),
        type="b",pch=16,xaxt="n",
        main="Expt. 6b (no commas)",cex.main=2,lwd=3,ylab="",
        cex.axis=1.8)

#lines(ab.means[3,],lwd=3,col="black",lty=1)
#points(ab.means[3,],pch=19,col="black"))

lines(ab.means[4,],lwd=3,col="black",lty=4)
points(ab.means[4,],pch=19,col="black")

arrows(seq(from=1,4,by=1),ci.lower.c,
       seq(from=1,4,by=1),ci.upper.c,
       lwd=2,col="black",angle=90,code=3,length=.1)

arrows(seq(from=1,4,by=1),ci.lower.d,
       seq(from=1,4,by=1),ci.upper.d,
       lwd =  2,col="black",angle=90,code=3,length=.1)

mtext(text=c("V3","V2","V1","Post-V1"),side=1,line=1.5,at=1:4,cex=2)
#mtext(text="Reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)

dev.off()


