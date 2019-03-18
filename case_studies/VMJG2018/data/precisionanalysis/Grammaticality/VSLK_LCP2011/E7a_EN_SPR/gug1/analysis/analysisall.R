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
alldataq <- read.table("alldatagug1q.txt")
colnames(alldataq) <- c("run","subject","subj","expt","item","condition","question","response","correct","qrt")

## remove practice items
alldataq <- subset(alldataq,expt!="practice")

## subset second run
#alldatsq <- subset(alldataq,run=="second")

## mean and se of all responses:
mean(with(alldataq,tapply(correct,subject,mean)))
se(with(alldataq,tapply(correct,subject,mean)))

alldataq.gug <- subset(alldataq,expt=="gug")
alldataq.gug$condition <- factor(alldataq.gug$condition)

## code up grammaticality factor:
alldataq.gug$gram <- ifelse(alldataq.gug$condition%in%c("a"),1,-1)

## means and se of engug expt, gram:
with(alldataq.gug,tapply(correct,condition,mean,na.rm=TRUE))*100
with(alldataq.gug,tapply(correct,condition,se))*100


means.subj.gug <- (with(alldataq.gug,tapply(correct,IND=list(subject,gram),mean,na.rm=TRUE))*100)
barplot(means.subj.gug[,2]-means.subj.gug[,1])

summary(lmer(correct~gram+(1|subject)+(1|item),family=binomial(),alldataq.gug))

## gram: pos 6,7,8,9
## ungram 6 7 8
data <- read.table("datagugnew.txt")

colnames(data) <- c("run","subj","subject","expt","item","condition","pos","word","dummy2","rt")

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


V1 <- crit[2,2]
postV1 <- crit[2,3]

crit[2,2] <- NA
crit[2,3] <- V1
crit[2,4] <- postV1

barplot(crit,beside=T)


## V3
(fm.v3 <- lmer(log(rt)~condition+(1|subj)+(1|item),subset(gram,pos==6 & rt<2000)))

(fm.v3 <- lmer(log(rt)~condition+(1|subj)+(1|item),subset(gram,pos==6)))

qq.plot(resid(fm.v3))

ifelse(gram$condition=="a",-1,1)

## V1
(fm.v1 <- lmer(log(rt)~condition+(1|subj)+(1|item),subset(gram,(condition=="a" & pos==8) | (condition=="b" & pos==7))))

(fm.v1 <- lmer(log(rt)~condition+(1|subj)+(1|item),subset(gram,(condition=="a" & pos==8) | (condition=="b" & pos==7 & rt<2000))))

qq.plot(resid(fm.v1))

## post-V1
(fm.postv1 <- lmer(log(rt)~condition+run+(1|subj)+(1|item),subset(gram,(condition=="a" & pos==9) | (condition=="b" & pos==8)  & rt<2000)))

(fm.postv1 <- lmer(log(rt)~condition+(1|subj)+(1|item),subset(gram,(condition=="a" & pos==9) | (condition=="b" & pos==8))))

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

matplot(ab.means[1,],
        ylim=c(min(ci.lower.b,na.rm=T)-50,max(ci.upper.a,na.rm=T)+50),
        type="b",pch=16,xaxt="n",
        main="Experiment 6 (English SPR)",cex.main=2,lwd=3,ylab="",
        cex.axis=1.8)

lines(ab.means[2,],lwd=3,col=gray(.5),lty=2)
points(ab.means[2,],pch=19,col=gray(.5))

arrows(seq(from=1,4,by=1),ci.lower.a,
       seq(from=1,4,by=1),ci.upper.a,
       lwd=2,col="black",angle=90,code=3,length=.1)

arrows(seq(from=1,4,by=1),ci.lower.b,
       seq(from=1,4,by=1),ci.upper.b,
       lwd =  2,col=gray(0.5),angle=90,code=3,length=.1)

mtext(text=c("V3","V2","V1","Post-V1"),side=1,line=1.5,at=1:4,cex=2)
mtext(text="Reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)

dev.off()


