## fexpt1 in Jan03Data is expt 2 in Lg 2006 paper
## fexpt4 is expt 3

expt2<-read.table("expt2dataVasishthLewisLg2006.txt",header=T)
expt3<-read.table("expt3dataVasishthLewisLg2006.txt",header=T)

head(expt2)

expt2$expt<-factor("e2")
expt3$expt<-factor("e3")
expt2$subject<-factor(paste(expt2$expt,expt2$subj,sep=""))
expt3$subject<-factor(paste(expt3$expt,expt3$subj,sep=""))
expt2$item<-factor(paste(expt2$expt,expt2$sent,sep=""))
expt3$item<-factor(paste(expt3$expt,expt3$group,sep=""))


data<-rbind(expt2[,c(4,5,6,10,11,12)],expt3[,c(4,5,6,9,10,11)])

RC<-ifelse(data$condfill%in%c("J","K"),"OR","SR")
Len<-ifelse(data$condfill%in%c("J","L"),"Long","Short")

c1.rc<-ifelse(data$condfill%in%c("J","K"),-1,1)
c2.len<-ifelse(data$condfill%in%c("J","L"),-1,1)

data$RC<-factor(RC)
data$Len<-factor(Len)

data$c1.rc<-c1.rc
data$c2.len<-c2.len

library(lme4)
library(MASS)

##log transform:
with(data,boxcox(raw~condfill*subject))

data$log.rt<-log(data$raw)

head(data)

fm0<-lmer(log.rt~c1.rc*c2.len+(1|subject)+(1|item),
          data)

library(car)
qqPlot(residuals(fm0))

contrasts(data$expt)<-contr.sum(2)

(fm1<-lmer(log.rt~c1.rc*c2.len*expt+(1|subject)+(1|item),data))

summary(fm1)
qqPlot(residuals(fm1))

(fm1.e2<-lmer(log.rt~c1.rc*c2.len+(1|subject)+(1|item),subset(data,expt=="e2")))
(fm1.e3<-lmer(log.rt~c1.rc*c2.len+(1|subject)+(1|item),subset(data,expt=="e3")))


means<-round(with(data,
                  tapply(raw,IND=list(RC,Len),mean)),digits=0)

barplot(means,beside=T)

means.2<-round(with(data,tapply(raw,IND=list(RC,Len,expt),
                                mean)),digits=0)

## 
round(with(data,tapply(raw,expt,mean)),digits=0)
