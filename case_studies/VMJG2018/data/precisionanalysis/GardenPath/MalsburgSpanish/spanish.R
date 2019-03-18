load("spanish_study.Rda")
load("cri.Rda")

length(unique(fe.em.rid2$subj))
length(unique(sinfos$subj))

dim(fe.em.rid2)
dim(sinfos)
dim(tinfos)

data<-merge(fe.em.rid2,sinfos,by.x="subj",by.y="subj")

data<-merge(data,tinfos[,c(1,2,11)],by.x=c("subj","tid"),by.y=c("subj","tid"))

dim(data)

xtabs(~item+cond,data)

data$cond<-as.character(data$cond)

data<-subset(data,cond!="-")

## if reanalysis is happening in b vs a,
## c easiest, then b, then a
## Define sliding contrasts with c as baseline:
data$cond<-factor(data$cond,levels=c("c","b","a"))

library(MASS)

#contrasts(data$cond)<-t(ginv(contr.helmert(3)))
contrasts(data$cond)<-contr.sdif(3)

contrasts(data$cond)

fpreg<-ifelse(data$RBRC==0,0,1)
data$fpreg<-fpreg

## re-reading probability
rrt.prob<-ifelse(data$RRT==0,0,1)
data$rrt.prob<-rrt.prob

## descriptive data for all regions:
## note that conditions are ordered
## c,b,a, so c occurs first.

## A "reanalysis pattern" would be c<b<a where 
## < is "faster than".

## something strange at cuando (roi 6, condition c):
barplot(t(with(data,tapply(FFD,IND=list(roi,cond),mean,na.rm=TRUE))),beside=T)

## slowdown at region 7 in easiest condition. Why?
barplot(t(with(data,tapply(FPRT,IND=list(roi,cond),mean,na.rm=TRUE))),beside=T)

## region 10 shows a clear pattern of reanalysis:
barplot(t(with(data,tapply(RPD,IND=list(roi,cond),mean,na.rm=TRUE))),beside=T)

barplot(t(with(data,tapply(RBRT,IND=list(roi,cond),mean,na.rm=TRUE))),beside=T)

## clear reanalysis pattern in region 9:
barplot(t(with(data,tapply(RRT,IND=list(roi,cond),mean,na.rm=TRUE))),beside=T)

## clear reanalysis pattern in 8 and 9, but look at 7:
## the unambiguous case is most costly:
barplot(t(with(data,tapply(fpreg,IND=list(roi,cond),mean,na.rm=TRUE))),beside=T)

## re-reading prob. at cuando, roi 6, is low in c:
barplot(t(with(data,tapply(rrt.prob,IND=list(roi,cond),mean,na.rm=TRUE))),beside=T)


## critical regions:
crit<-subset(data,roi%in%c(7,8,9,10))

crit$acc<-as.numeric(as.character(crit$acc))

## make wm-span a three way distinction for visualization:
quants<-cut(crit$pcu,breaks=3,labels=c("low","med","high"))
crit$pcu2<-quants


library(lme4)

## c and b have same accuracy, a is lower than b:
with(subset(crit,roi==8),tapply(acc,cond,mean))
lmer(acc~cond+(1|subj)+(1|item),family=binomial(),subset(data,roi==8))

## result 1:
## c is processed slower than b in roi 7, a pof effect?
with(crit,tapply(FPRT,IND=list(cond,roi),mean))
lmer(log(FPRT+1)~cond*scale(pcu,scale=FALSE)+(1|subj)+(1|item),subset(crit,roi==7))

## interaction of WM with reanalysis difficulty:
barplot(with(subset(crit,roi==7),tapply(FPRT,IND=list(cond,pcu2),mean)),beside=T)

## same pattern as FPRT at position 7!
with(crit,tapply(RPD,IND=list(cond,roi),mean))
lmer(log(RPD+1)~cond*scale(pcu,scale=FALSE)+(1|subj)+(1|item),subset(crit,roi==7))

## looks like the high spans regress a lot more from region 7
## in the unambiguous baseline:
barplot(with(subset(crit,roi==7),tapply(RPD,IND=list(cond,pcu2),mean)),beside=T)

with(crit,tapply(RBRT,IND=list(cond,roi),mean))
lmer(log(RBRT+1)~cond*scale(pcu,scale=FALSE)+(1|subj)+(1|item),subset(crit,roi==7))

## same pattern at roi 7 as FPRT, RPD:
barplot(with(subset(crit,roi==7),tapply(RBRT,IND=list(cond,pcu2),mean)),beside=T)

## at roi 7, unambiguous c is slower than b;
## but a is slower than b as expected.
with(crit,tapply(RRT,IND=list(cond,roi),mean))
lmer(log(RRT+1)~cond*scale(pcu,scale=FALSE)+(1|subj)+(1|item),subset(crit,roi==7))

## now we are seeing at the critical region that
## b is slower than c, as we'd expected (the reanalysis effect)
## it seems to be a later-stage effect.
lmer(log(RRT+1)~cond*scale(pcu,scale=FALSE)+(1|subj)+(1|item),subset(crit,roi==8))

## in roi 7, more regressions in unambiguous case than b.
## classic reanalysis pattern in roi 9.
## lots of regressions at pos 10, re-reading?
with(crit,tapply(fpreg,IND=list(cond,roi),mean))
lmer(fpreg~cond*scale(pcu,scale=FALSE)+(1|subj)+(1|item),family=binomial(),subset(crit,roi==7))

lmer(fpreg~cond*scale(pcu,scale=FALSE)+(1|subj)+(1|item),family=binomial(),subset(crit,roi==8))

## approaches significance: c<b
lmer(fpreg~cond*scale(pcu,scale=FALSE)+(1|subj)+(1|item),family=binomial(),subset(crit,roi==9))

with(crit,tapply(rrt.prob,IND=list(cond,roi),mean))
lmer(rrt.prob~cond*scale(pcu,scale=FALSE)+(1|subj)+(1|item),family=binomial(),subset(crit,roi==7))

## marginal interaction with wm:
lmer(rrt.prob~cond*scale(pcu,scale=FALSE)+(1|subj)+(1|item),family=binomial(),subset(crit,roi==8))

barplot(with(subset(crit,roi==8),tapply(rrt.prob,IND=list(cond,pcu2),mean)),beside=T)

## cri:

head(fe.em.rid2.cri)


data.cri<-merge(fe.em.rid2.cri,sinfos,by.x="subj",by.y="subj")

data.cri<-merge(data.cri,tinfos[,c(1,2,11)],by.x=c("subj","tid"),by.y=c("subj","tid"))

data.cri$cond<-as.character(data.cri$cond)

data.cri<-subset(data.cri,cond!="-")

quants<-cut(data.cri$pcu,breaks=3,labels=c("low","med","high"))
data.cri$pcu2<-quants

with(subset(data.cri,srcRoi==8),tapply(CFC,IND=list(cond,dstRoi),sum))

with(subset(data.cri,srcRoi==9),tapply(CFC,IND=list(cond,dstRoi),sum))


with(subset(data.cri,srcRoi==10),tapply(CFC,IND=list(cond,dstRoi),sum))

