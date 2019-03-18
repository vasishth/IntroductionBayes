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
alldataq <- read.table("gug1/allquestionsgug1.txt")
colnames(alldataq) <- c("subject","expt","item","condition","question","response","correct","qrt")

## remove practice items
alldataq <- subset(alldataq,expt!="practice")

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
data <- read.table("gug1/alldatagug1.txt")

colnames(data) <- c("subj","expt","item","condition","pos","word","dummy2","rt")

data.gug <- subset(data,expt=="gug")

summary(data.gug)

data.gug$expt <- factor(data.gug$expt)

data.gug$condition <- factor(data.gug$condition)

data.gug$pos <- data.gug$pos+1

gram <- subset(data.gug,pos%in%c(6,7,8,9))

crit <- with(gram,tapply(rt,IND=list(condition,pos),mean))

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

## V1
(fm.v1 <- lmer(rt~condition+(1|subj)+(1|item),subset(gram,(condition=="a" & pos==8) | (condition=="b" & pos==7)  & rt<800)))

(fm.v1 <- lmer(log(rt)~condition+(1|subj)+(1|item),subset(gram,(condition=="a" & pos==8) | (condition=="b" & pos==7 & rt<800))))


qq.plot(resid(fm.v1))

## post-V1
(fm.postv1 <- lmer(log(rt)~condition+(1|subj)+(1|item),subset(gram,(condition=="a" & pos==9) | (condition=="b" & pos==8)  & rt<800)))

(fm.postv1 <- lmer(log(rt)~condition+(1|subj)+(1|item),subset(gram,(condition=="a" & pos==9) | (condition=="b" & pos==8))))

qq.plot(resid(fm.postv1))
