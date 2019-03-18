setwd("C:/Users/Farnoosh/Desktop")

data1<-read.table("Pretest-1-sentence completion/pretest1.csv", header=T, sep = "\t")

colnames(data1)<-c("exp", "group","subj","item","cond","resp")

head(data1)

data1$resp<-factor(data1$resp)
data1$cond<-factor(data1$cond)

exp1 <- subset(data1, exp==1)
exp2 <- subset(data1, exp==2)

#response (experiment 1)

exp1$resp<- as.numeric(as.character(exp1$resp))
means<-round(100*with(exp1, tapply(resp,cond,mean)), digits=0)
means
barplot(means,beside=T)

#response (experiment 2)

exp2$resp<- as.numeric(as.character(exp2$resp))
means2<-round(100*with(exp2, tapply(resp,cond,mean)), digits=0)
means2
barplot(means2,beside=T)

## short -1, long 1
exp1$dist<-ifelse(exp1$cond%in%c("a","c"),-1,1)
## pred 1, -1
exp1$pred<-ifelse(exp1$cond%in%c("a","b"),-1,1)

#nested
exp1$pred.dist <- ifelse(exp1$cond=="a",-1,
                          ifelse(exp1$cond=="b",1,0))
exp1$nopred.dist <- ifelse(exp1$cond=="c",-1,
                            ifelse(exp1$cond=="d",1,0))

library(lme4)

#main effect of prediction (light verb predictions significantly higher in a,b than c,d)
#no effect of distance
#no interaction

m1<-glmer(resp~dist*pred+(1|subj)+(1|item),exp1,family=binomial())
summary(m1)



## short -1, long 1
exp2$dist<-ifelse(exp2$cond%in%c("a","c"),-1,1)
## pred 1, -1
exp2$pred<-ifelse(exp2$cond%in%c("a","b"),-1,1)

#nested
exp2$pred.dist <- ifelse(exp2$cond=="a",-1,
                         ifelse(exp2$cond=="b",1,0))
exp2$nopred.dist <- ifelse(exp2$cond=="c",-1,
                           ifelse(exp2$cond=="d",1,0))


#results same as experiment 1 sentence completion

#main effect of prediction (light verb predictions significantly higher in a,b than c,d)
#no effect of distance
#no interaction

m2<-glmer(resp~dist*pred+(1|subj)+(1|item),exp2,family=binomial())
summary(m2)

#####################################################################
#pretest 2 analysis (acceptability judgement)

data2<-read.table("Pretest-2-acceptability-rating(sep-vs-insep)/pretest2.csv", header=T, sep = "\t")

head(data2)

data2$cond<-factor(data2$cond)

summary(data2)

xtabs(~item+cond,data2)
xtabs(~subj+cond,data2)


data2.item <- subset(data2, cond!='filler')
data2.item$cond <- droplevels(data2.item$cond)
#response (experiment 1)

data2.item$response<- as.numeric(as.character(data2.item$response))

boxplot(response~cond,data2.item)

library(xtable)
xtable(t(round(with(data2.item,tapply(response,IND=list(cond,item),mean)),digits=1)))

means<-round(with(data2.item, tapply(response,cond,mean)), digits=0)
means

#condition b is the most acceptable
barplot(means,beside=T)

head(data2.item)

summary(data2.item$response)

xtabs(~subj+item,data2.item)

round(with(data2.item, tapply(response,IND=list(cond,item),mean)),digits=0)

with(data2.item,tapply(response,cond,summary))

hist(subset(data2.item, cond=='a')$response)
hist(subset(data2.item, cond=='b')$response)
hist(subset(data2.item, cond=='c')$response)
hist(subset(data2, cond=='filler')$response)

xtabs(~cond+item,data2.item)

library(lattice)
xyplot(response~cond|factor(item),data2.item)

data2.item$cond<-factor(data2.item$cond,levels=c("b","a","c"))

contrasts(data2.item$cond)

library(lme4)

data2.item$item2<-factor(paste(data2.item$item,
                               data2.item$cond,
                               sep=""))

xtabs(~item2+cond,data2.item)

M0<-lmer(response~cond+(1|subj), data2.item)

summary(M0)
library(car)
qqPlot(residuals(M0))
