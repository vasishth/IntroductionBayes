## ----loadlibraries, echo=FALSE, include=FALSE----------------------------
library(lme4)
#library(RePsychLing)
#library(car)
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(lattice)
library(xtable)
library(MASS)

source("../R/multiplot.R")
source("../R/plotresults.R")
source("../R/coeflmer.R")
source("../R/regionmeans.R")
source("../R/regionplot.R")
source("../R/magnifytext.R")

## ---- echo = FALSE-------------------------------------------------------
      knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----preprocesse1e2,echo=FALSE,include=FALSE,eval=FALSE------------------
## e1<-read.table("../data/Expt1.txt",header=TRUE)
## 
## ## create region table:
## ## recode regions of interest:
## region<-c(0:6,0:7,0:7,0:8)
## length(region)
## cond<-rep(letters[1:4],c(7,8,8,9))
## condaroi<-c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO")
## condbroi<-c("CL","RCSubj","RCV","de","headnoun","Adv","MainV","MainO")
## condcroi<-c("BEI","RCSubj","RCV","de","headnoun","Adv","MainV","MainO")
## conddroi<-c("CL","BEI","RCSubj","RCV","de","headnoun","Adv","MainV","MainO")
## 
## condroi<-c(condaroi,condbroi,condcroi,conddroi)
## regiontable<-data.frame(cond=cond,reg=region,roi=condroi)
## 
## e1temp<-merge(e1,regiontable,by.x=c("condition","region"),by.y=c("cond","reg"))
## 
## e1<-e1temp
## 
## e1$roi<-factor(e1$roi,levels=c("CL","BEI","RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))
## 
## e1$ID<-factor(paste("e1_",e1$ID,sep=""))
## colnames(e1)[3]<-"subject"
## e1$expt<-"e1"
## e1<-e1[,c(3,5,1,10,11,12)]
## #write.table(e1,file="e1clean.txt")
## 
## e1q<-read.table("../data/Expt1.txt",header=TRUE)
## e1q<-subset(e1q,region=="?")
## colnames(e1q)[1]<-"subject"
## e1q$subject<-factor(paste("e1_",e1q$subject,sep=""))
## e1q<-e1q[,c(1,3,4,9,10)]
## e1q$expt<-"e1"
## 
## head(e1q)
## e1q<-e1q[,c(1,2,3,4)]
## 
## #dim(e1)
## e1merged<-merge(e1,e1q,by.x=c("subject","item","condition"),
##       by.y=c("subject","item","condition"))
## #dim(e1merged)
## #head(e1merged)
## 
## #head(e1merged)
## e1merged<-e1merged[,c(1,2,3,4,7,5,6)]
## 
## write.table(e1merged,file="e1clean.txt")
## 
## ## e2
## e2<-read.csv("../data/Expt2.csv",header=TRUE)
## e2$sub<-factor(paste("e2_",e2$sub,sep=""))
## e2$item <- as.factor(e2$item)
## e2$pos <- as.factor(e2$pos)
## e2$posW <- as.character(e2$posW)
## ## relabel pos coding:
## #"RCSubj","RCV","de","headnoun","Adv","MainV","MainO"
## ## 2 embedded N RCSubj
## ## 3 rc verb RCV
## ## 4 de de
## ## 5 head noun headnoun
## ## 6 head noun+1: adv Adv
## ## 7 main verb MainV
## ## 8 main obj MainO
## e2$roi<-ifelse(e2$relabelpos==2,"RCSubj",ifelse(e2$relabelpos==3,"RCV",
## ifelse(e2$relabelpos==4,"de",
## ifelse(e2$relabelpos==5,"headnoun",
## ifelse(e2$relabelpos==6,"Adv",
## ifelse(e2$relabelpos==7,"MainV",
## ifelse(e2$relabelpos==8,"MainO",NA)))))))
## 
## head(e2)
## colnames(e2)[1]<-"subject"
## colnames(e2)[4]<-"condition"
## e2<-e2[,c(1,3,4,8,9,13)]
## head(e2)
## 
## e2$expt<-"e2"
## 
## colnames(e2)[5]<-"correct"
## 
## write.table(e2,file="e2clean.txt")

## ----loade1--------------------------------------------------------------
e1<-read.table("../data/e1clean.txt",header=TRUE)

## ----analysise1----------------------------------------------------------
e1$CL<-ifelse(e1$condition%in%c("a","c"),-1,1)
e1$BEI<-ifelse(e1$condition%in%c("a","b"),-1,1)
e1$CLxBEI<-ifelse(e1$condition%in%c("b","c"),-1,1)

## nested contrasts:
## condition a: no cl no bei
## condition b: cl, no bei (regions are moved forward by 1) cl in region 0
## condition c: no cl, bei (regions are moved forward by 1) bei in region 0
## condition d: cl, be (regions are moved forward by 1), cl in region 0, bei in region 1

#       a    b     c     d
#Bei1   -1   0     1     0
#Bei2   0    -1    0     1
#CL    -1   1    -1      1

e1$BEI_inNoCL <- ifelse(e1$condition=="a",-1,
                     ifelse(e1$condition=="c",1,0))
e1$BEI_inCL <- ifelse(e1$condition=="b",-1,
                  ifelse(e1$condition=="d",1,0))

## 
boxplot(RT~condition,e1)
boxplot(-1000/RT~condition,e1)
boxcox(e1$RT~e1$condition*e1$subject)

e1$rrt<- -1000/e1$RT ## reciprocal
e1$lrt<- log(e1$RT)  ## log rt

#xtabs(~CL+condition,e1)
#xtabs(~BEI+condition,e1)
#xtabs(~CLxBEI+condition,e1)

#xtabs(~CL+item,e1)
#xtabs(~BEI+ID,e1)
#tabs(~CLxBEI+item,e1)

## RCSubj no evidence for any effects
m1rrt<-lmer(rrt~CL+BEI+CLxBEI+
           (1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e1,roi=="RCSubj"))
round(summary(m1rrt)$coefficients,digits=3)

m1lrt<-lmer(lrt~CL+BEI+CLxBEI+
           (1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e1,roi=="RCSubj"))
round(summary(m1lrt)$coefficients,digits=3)

m1rt<-lmer(RT~CL+BEI+CLxBEI+
           (1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e1,roi=="RCSubj"))
round(summary(m1lrt)$coefficients,digits=3)


##RCV no effects
m2rrt<-lmer(rrt~CL+BEI+CLxBEI+(1|subject)+
           (1|item),subset(e1,roi=="RCV"))
summary(m2rrt)
m2lrt<-lmer(lrt~CL+BEI+CLxBEI+(1|subject)+
           (1|item),subset(e1,roi=="RCV"))
summary(m2lrt)

m2rt<-lmer(RT~CL+BEI+CLxBEI+(1|subject)+
           (1|item),subset(e1,roi=="RCV"))
summary(m2lrt)


library(car)
qqPlot(residuals(m2rrt))
qqPlot(residuals(m2lrt))

##de 
## rrt: main effects and interactions
m3rrt<-lmer(rrt~CL+BEI+CLxBEI+(1+BEI+CLxBEI||subject)+
           (1+CL+BEI||item),
         subset(e1,roi=="de"))
summary(m3rrt)

## ------------------------------------------------------------------------
## 
op<-par(mfrow=c(2,3),pty="s")
barplot(round(with(subset(e1,roi=="de"),tapply(rrt,condition,mean)),digits=2),beside=TRUE,main="Reciprocal RTs (-1000/RT)",cex.axis=2,cex.main=2,cex=2)
barplot(round(with(subset(e1,roi=="de"),tapply(lrt,condition,mean)),digits=2),beside=TRUE,main="Log RTs",cex.axis=2,cex.main=2,cex=2)
barplot(round(with(subset(e1,roi=="de"),tapply(RT/1000,condition,mean)),digits=2),beside=TRUE,main="Raw RTs (seconds)",cex.axis=2,cex.main=2,cex=2)
boxplot(rrt~condition,subset(e1,roi=="de"),cex.axis=2,cex.main=2,cex=2,main="Reciprocal RTs (-1000/RT)")
boxplot(lrt~condition,subset(e1,roi=="de"),cex.axis=2,cex.main=2,cex=2,main="Log RTs")
boxplot(RT/1000~condition,subset(e1,roi=="de"),cex.axis=2,cex.main=2,cex=2,main="Raw RTs (seconds)")

## ------------------------------------------------------------------------
## lrt no evidence for interaction
m3lrt<-lmer(lrt~CL+BEI+CLxBEI+(1+BEI+CLxBEI||subject)+
           (1+CL+BEI||item),
         subset(e1,roi=="de"))
summary(m3lrt)

## raw RTs: no effect
m3rt<-lmer(RT~CL+BEI+CLxBEI+(1+BEI+CLxBEI||subject)+
           (1+CL+BEI||item),
         subset(e1,roi=="de"))
summary(m3rt)

## nested comparison:
#xtabs(~BEI_inNoCL + condition, subset(e1,roi=="de"))
#xtabs(~BEI_inCL + condition, subset(e1,roi=="de"))
#xtabs(~BEI + condition, subset(e1,roi=="de"))

m3nested<-lmer(rrt~BEI_inNoCL + BEI_inCL + CL + (1+BEI_inNoCL + BEI_inCL ||subject)+
                 (CL||item),
               subset(e1,roi=="de"))

round(with(subset(e1,roi=="de"),tapply(RT,condition,mean)))

summary(m3nested)

##headnoun no effects
m4rrt<-lmer(rrt~CL+BEI+CLxBEI+(1+CLxBEI||subject)+
           (1|item),
         subset(e1,roi=="headnoun"))
summary(m4rrt)

m4lrt<-lmer(lrt~CL+BEI+CLxBEI+(1+CLxBEI||subject)+
           (1|item),
         subset(e1,roi=="headnoun"))
summary(m4lrt)

m4rt<-lmer(RT~CL+BEI+CLxBEI+(1+CLxBEI||subject)+
           (1|item),
         subset(e1,roi=="headnoun"))
summary(m4rt)


qqPlot(residuals(m4rrt))
qqPlot(residuals(m4lrt))
qqPlot(residuals(m4rt))

boxplot(RT~condition,subset(e1,roi=="headnoun"))

##Adv main effects
m5rrt<-lmer(rrt~CL+BEI+CLxBEI+(1|subject)+
           (1+CL+BEI||item),
         subset(e1,roi=="Adv"))
summary(m5rrt)

qqPlot(residuals(m5rrt))

m5lrt<-lmer(lrt~CL+BEI+CLxBEI+(1|subject)+
           (1+CL+BEI||item),
         subset(e1,roi=="Adv"))
summary(m5lrt)

qqPlot(residuals(m5lrt))

m5rt<-lmer(RT~CL+BEI+CLxBEI+(1|subject)+
           (1+CL+BEI||item),
         subset(e1,roi=="Adv"))
summary(m5rt)

qqPlot(residuals(m5rt))

## post-hoc analysis de+headnoun summed:

dee1<-subset(e1,roi=="de")
hne1<-subset(e1,roi=="headnoun")
#dim(dee1)
#dim(hne1)

dee1$dehn<-dee1$RT+hne1$RT

dee1$rrt_dehn<- -1000/dee1$dehn
dee1$lrt_dehn<- log(dee1$dehn)

m5rrtdehn<-lmer(rrt_dehn~CL+BEI+CLxBEI+
           (1+CLxBEI||subject)+
           (1+CL+CLxBEI||item),
            dee1)
summary(m5rrtdehn)
qqPlot(residuals(m5rrtdehn))

m5lrtdehn<-lmer(lrt_dehn~CL+BEI+CLxBEI+
           (1+CLxBEI||subject)+
           (1+CL+CLxBEI||item),
            dee1)
summary(m5lrtdehn)
qqPlot(residuals(m5lrtdehn))

m5rtdehn<-lmer(dehn~CL+BEI+CLxBEI+
           (1+CLxBEI||subject)+
           (1+CL+CLxBEI||item),
            dee1)
summary(m5rtdehn)
qqPlot(residuals(m5rtdehn))


##MainV main effects
m6rrt<-lmer(rrt~CL+BEI+CLxBEI+
           (1|subject)+
           (1+BEI+CLxBEI||item),
         subset(e1,roi=="MainV" & rrt>-5))
summary(m6rrt)

boxplot(rrt~condition,subset(e1,roi=="MainV"))

qqPlot(residuals(m6rrt))

m6lrt<-lmer(lrt~CL+BEI+CLxBEI+
           (1|subject)+
           (1+BEI+CLxBEI||item),
         subset(e1,roi=="MainV"))
summary(m6lrt)

qqPlot(residuals(m6lrt))

m6rt<-lmer(RT~CL+BEI+CLxBEI+
           (1|subject)+
           (1+BEI+CLxBEI||item),
         subset(e1,roi=="MainV"))
summary(m6rt)

qqPlot(residuals(m6lrt))


##MainO main effects CL effect
m7rrt<-lmer(rrt~CL+BEI+CLxBEI+(1+BEI+CLxBEI||subject)+
           (1|item),
         subset(e1,roi=="MainO"))
summary(m7rrt)
qqPlot(residuals(m7rrt))

m7lrt<-lmer(lrt~CL+BEI+CLxBEI+(1+BEI+CLxBEI||subject)+
           (1|item),
         subset(e1,roi=="MainO" & rrt>-5))
summary(m7lrt)
qqPlot(residuals(m7lrt))

m7rt<-lmer(RT~CL+BEI+CLxBEI+(1+BEI+CLxBEI||subject)+
           (1|item),
         subset(e1,roi=="MainO" & rrt>-5))
summary(m7rt)
qqPlot(residuals(m7rt))

## ------------------------------------------------------------------------
resultse1rrt<-rbind(coeflmer(m1rrt), coeflmer(m2rrt), coeflmer(m3rrt),
      coeflmer(m4rrt), coeflmer(m5rrt), coeflmer(m6rrt), coeflmer(m7rrt))
contrasts<-rownames(resultse1rrt)
contrasts<-factor(contrasts,levels=c("CL","BEI","CLxBEI"))

resultse1rrt<-data.frame(resultse1rrt)
colnames(resultse1rrt)<-c("estimate","se","t")
resultse1rrt<-cbind(contrasts,resultse1rrt)

rois<-rep(c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"),each=3)
rois<-factor(rois,levels=c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))
resultse1rrt<-cbind(resultse1rrt,rois)

resultse1rrt$lower<-resultse1rrt$estimate-2*resultse1rrt$se
resultse1rrt$upper<-resultse1rrt$estimate+2*resultse1rrt$se
resultse1rrt$expt<-"e1"

## ------------------------------------------------------------------------
resultse1rrt[which(abs(resultse1rrt$t) >= 2.5),c(5,1,2,3,4)]

## ----fig.width=11,fig.height=10------------------------------------------
figresultse1rrt<-plotresults(resultse1rrt,maintitle="Experiment 1")
print(figresultse1rrt)

## ------------------------------------------------------------------------
resultse1<-rbind(coeflmer(m1lrt), coeflmer(m2lrt), coeflmer(m3lrt),
      coeflmer(m4lrt), coeflmer(m5lrt), coeflmer(m6lrt), coeflmer(m7lrt))
contrasts<-rownames(resultse1)
contrasts<-factor(contrasts,levels=c("CL","BEI","CLxBEI"))

resultse1<-data.frame(resultse1)
colnames(resultse1)<-c("estimate","se","t")
resultse1<-cbind(contrasts,resultse1)

rois<-rep(c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"),each=3)
rois<-factor(rois,levels=c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))
resultse1<-cbind(resultse1,rois)

resultse1$lower<-resultse1$estimate-2*resultse1$se
resultse1$upper<-resultse1$estimate+2*resultse1$se
resultse1$expt<-"e1"

## ----fig.width=11,fig.height=10------------------------------------------
figresultse1lrt<-plotresults(resultse1,maintitle="Experiment 1",
                             ylabel="Log RT")
print(figresultse1lrt)

## ------------------------------------------------------------------------
resultse1<-rbind(coeflmer(m1rt), coeflmer(m2rt), coeflmer(m3rt),
      coeflmer(m4rt), coeflmer(m5rt), coeflmer(m6rt), coeflmer(m7rt))
contrasts<-rownames(resultse1)
contrasts<-factor(contrasts,levels=c("CL","BEI","CLxBEI"))

resultse1<-data.frame(resultse1)
colnames(resultse1)<-c("estimate","se","t")
resultse1<-cbind(contrasts,resultse1)

rois<-rep(c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"),each=3)
rois<-factor(rois,levels=c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))
resultse1<-cbind(resultse1,rois)

resultse1$lower<-resultse1$estimate-2*resultse1$se
resultse1$upper<-resultse1$estimate+2*resultse1$se
resultse1$expt<-"e1"

## ----fig.width=11,fig.height=10------------------------------------------
figresultse1<-plotresults(resultse1,maintitle="Experiment 1",
                          ylabel="Raw RTs")
print(figresultse1)

## ------------------------------------------------------------------------
e1crit<-subset(e1,roi%in%c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))

colnames(e1crit)[4]<-"rt"

e1crit$roi<-as.factor(as.character(e1crit$roi))
#unique(e1crit$roi)

e1crit$roi<-factor(e1crit$roi,levels=c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))

M.id.w<-regionmeans(e1crit)

## ----results="asis"------------------------------------------------------
library(xtable)
print(xtable(M.id.w[,c(1,2,6,7,3,4)],digits=c(0,0,0,0,0,0,0)),
      type="html")

## ----fig.width=11,fig.height=10------------------------------------------
M.id.w$condition<-factor(paste(M.id.w$condition,M.id.w$classifier,M.id.w$bei,sep=" "))

levels(M.id.w$condition)<-c("a NoCL NoBEI", "b CL NoBEI", "c NoCL BEI", "d CL BEI")

regionplot(M.id.w,maintitle="Experiment 1")

## ----fig.width=8,fig.height=8--------------------------------------------
multiplot(plotresults(resultse1rrt,maintitle="Experiment 1"),regionplot(M.id.w,maintitle="Experiment 1"),cols=1)

## ----loade2--------------------------------------------------------------
e2<-read.table("../data/e2clean.txt",header=TRUE)

## ----analysise2----------------------------------------------------------
e2$CL<-ifelse(e2$condition%in%c("a","c"),-1,1)
e2$BEI<-ifelse(e2$condition%in%c("a","b"),-1,1)
e2$CLxBEI<-ifelse(e2$condition%in%c("b","c"),-1,1)

## nested contrasts:
## condition a: no cl no bei
## condition b: cl, no bei (regions are moved forward by 1) cl in region 0
## condition c: no cl, bei (regions are moved forward by 1) bei in region 0
## condition d: cl, be (regions are moved forward by 1), cl in region 0, bei in region 1

#       a    b     c     d
#Bei1   -1   0     1     0
#Bei2   0    -1    0     1
#CL    -1   1    -1      1

e2$BEI_inNoCL <- ifelse(e2$condition=="a",-1,
                     ifelse(e2$condition=="c",1,0))
e2$BEI_inCL <- ifelse(e2$condition=="b",-1,
                  ifelse(e2$condition=="d",1,0))

#dim(e2)

e2$rrt<- -1000/e2$rt
e2$lrt<- log(e2$rt)


#summary(e2$rt)

dim(subset(e2,roi=="de"))
dim(subset(e2,roi=="headnoun")) ## one row less

## needed later for post-hoc analysis:
dee2<-subset(e2,roi=="de")
hne2<-subset(e2,roi=="headnoun")

dee2<-dee2[,c(1,2,3,4)]
hne2<-hne2[,c(1,2,3,4)]
colnames(hne2)[4]<-"rt2"

dehne2<-merge(dee2,hne2,
      by.x=c("subject","item","condition"),
      by.y=c("subject","item","condition"))

dim(dehne2)

dehne2$rt_dehn<-dehne2$rt+dehne2$rt2
dehne2$rrt_dehn<- -1000/dehne2$rt_dehn
dehne2$lrt_dehn<- log(dehne2$rt_dehn)

dehne2$CL<-ifelse(dehne2$condition%in%c("a","c"),-1,1)
dehne2$BEI<-ifelse(dehne2$condition%in%c("a","b"),-1,1)
dehne2$CLxBEI<-ifelse(dehne2$condition%in%c("b","c"),-1,1)

m_dehn<-lmer(rrt_dehn~CL+BEI+CLxBEI+(1+CL+CLxBEI||subject)+(1+BEI||item),dehne2)

summary(m_dehn)

#dim(dee2)
#dim(hne2)

e2<-subset(e2,rt>150)
#dim(e2)
#100*(11136-11128)/11136
## .07% of the data removed

## RCSubj
m1e2rrt<-lmer(rrt~CL+BEI+CLxBEI+
           (1+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e2,roi=="RCSubj"))
summary(m1e2rrt)
qqPlot(residuals(m1e2rrt))

m1e2lrt<-lmer(lrt~CL+BEI+CLxBEI+
           (1+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e2,roi=="RCSubj"))
summary(m1e2lrt)
qqPlot(residuals(m1e2lrt))

m1e2rt<-lmer(rt~CL+BEI+CLxBEI+
           (1+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e2,roi=="RCSubj"))
summary(m1e2rt)
qqPlot(residuals(m1e2rt))


##RCV 
m2e2rrt<-lmer(rrt~CL+BEI+CLxBEI+(1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),subset(e2,roi=="RCV" & rt>150))
summary(m2e2rrt)
qqPlot(residuals(m2e2rrt))

m2e2lrt<-lmer(lrt~CL+BEI+CLxBEI+(1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),subset(e2,roi=="RCV" & rt>150))
summary(m2e2lrt)
qqPlot(residuals(m2e2lrt))

m2e2rt<-lmer(rt~CL+BEI+CLxBEI+(1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),subset(e2,roi=="RCV" & rt>150))
summary(m2e2rt)
qqPlot(residuals(m2e2lrt))


##de main effects and interactions
m3e2rrt<-lmer(rrt~CL+BEI+CLxBEI+(1+CLxBEI||subject)+
           (1+CLxBEI||item),
         subset(e2,roi=="de"))
summary(m3e2rrt)

m3e2lrt<-lmer(lrt~CL+BEI+CLxBEI+(1+CLxBEI||subject)+
           (1+CLxBEI||item),
         subset(e2,roi=="de"))
summary(m3e2lrt)

m3e2rt<-lmer(rt~CL+BEI+CLxBEI+(1+CLxBEI||subject)+
           (1+CLxBEI||item),
         subset(e2,roi=="de"))
summary(m3e2rt)


## nested comparison:
#xtabs(~BEI_inNoCL + condition, subset(e1,roi=="de"))
#xtabs(~BEI_inCL + condition, subset(e1,roi=="de"))
#xtabs(~BEI + condition, subset(e1,roi=="de"))

m3nested<-lmer(rrt~BEI_inNoCL + BEI_inCL + CL + 
                   (1 + BEI_inCL ||subject)+
                 (1 |item),
               subset(e2,roi=="de"))

summary(m3nested)

##headnoun
m4e2rrt<-lmer(rrt~CL+BEI+CLxBEI+(1+CL+CLxBEI||subject)+
           (1+BEI||item),
         subset(e2,roi=="headnoun"))
summary(m4e2rrt)
qqPlot(residuals(m4e2rrt))

m4e2lrt<-lmer(lrt~CL+BEI+CLxBEI+(1+CL+CLxBEI||subject)+
           (1+BEI||item),
         subset(e2,roi=="headnoun"))
summary(m4e2lrt)
qqPlot(residuals(m4e2rrt))

m4e2rt<-lmer(rt~CL+BEI+CLxBEI+(1+CL+CLxBEI||subject)+
           (1+BEI||item),
         subset(e2,roi=="headnoun"))
summary(m4e2rt)
qqPlot(residuals(m4e2rt))

## post-hoc de+headnoun analysis:
m4rrtdehn<-lmer(rrt_dehn~CL+BEI+CLxBEI+(1+CLxBEI||subject)+
               (1|item),dehne2)
summary(m4rrtdehn)
qqPlot(residuals(m4rrtdehn))

#m4rrtdehn<-lmer(rrt_dehn~CL+BEI+CLxBEI+(1|subject)+
#               (1|item),dehne2)


##Adv main effects
m5e2rrt<-lmer(rrt~CL+BEI+CLxBEI+(1+BEI+CLxBEI||subject)+
           (1+CL||item),
         subset(e2,roi=="Adv"))
summary(m5e2rrt)

m5e2lrt<-lmer(lrt~CL+BEI+CLxBEI+(1+BEI+CLxBEI||subject)+
           (1+CL||item),
         subset(e2,roi=="Adv"))
summary(m5e2lrt)

m5e2rt<-lmer(rt~CL+BEI+CLxBEI+(1+BEI+CLxBEI||subject)+
           (1+CL||item),
         subset(e2,roi=="Adv"))
summary(m5e2rt)


#qqPlot(residuals(m5))

##MainV main effects
m6e2rrt<-lmer(rrt~CL+BEI+CLxBEI+
           (1+BEI+CLxBEI||subject)+
           (1+CL+BEI||item),
         subset(e2,roi=="MainV"))
summary(m6e2rrt)

m6e2lrt<-lmer(lrt~CL+BEI+CLxBEI+
           (1+BEI+CLxBEI||subject)+
           (1+CL+BEI||item),
         subset(e2,roi=="MainV"))
summary(m6e2lrt)

m6e2rt<-lmer(rt~CL+BEI+CLxBEI+
           (1+BEI+CLxBEI||subject)+
           (1+CL+BEI||item),
         subset(e2,roi=="MainV"))
summary(m6e2rt)


#qqPlot(residuals(m6))

##MainO main effects CL effect
m7e2rrt<-lmer(rrt~CL+BEI+CLxBEI+(1+CLxBEI||subject)+
           (1+CL||item),
         subset(e2,roi=="MainO"))
summary(m7e2rrt)

m7e2lrt<-lmer(lrt~CL+BEI+CLxBEI+(1+CLxBEI||subject)+
           (1+CL||item),
         subset(e2,roi=="MainO"))
summary(m7e2lrt)

m7e2rt<-lmer(rt~CL+BEI+CLxBEI+(1+CLxBEI||subject)+
           (1+CL||item),
         subset(e2,roi=="MainO"))
summary(m7e2rt)


qqPlot(residuals(m7e2rrt))

## ------------------------------------------------------------------------
resultse2rrt<-rbind(coeflmer(m1e2rrt), coeflmer(m2e2rrt), coeflmer(m3e2rrt),
      coeflmer(m4e2rrt), coeflmer(m5e2rrt), coeflmer(m6e2rrt), coeflmer(m7e2rrt))
contrasts<-rownames(resultse2rrt)
contrasts<-factor(contrasts,levels=c("CL","BEI","CLxBEI"))

resultse2rrt<-data.frame(resultse2rrt)
colnames(resultse2rrt)<-c("estimate","se","t")
resultse2rrt<-cbind(contrasts,resultse2rrt)

rois<-rep(c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"),each=3)
rois<-factor(rois,levels=c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))
resultse2rrt<-cbind(resultse2rrt,rois)

resultse2rrt$lower<-resultse2rrt$estimate-2*resultse2rrt$se
resultse2rrt$upper<-resultse2rrt$estimate+2*resultse2rrt$se
resultse2rrt$expt<-"e2"

## ------------------------------------------------------------------------
## comparing results:
subset(resultse1rrt,abs(t)>2.4)[,c(5,1,2,3,4)]
subset(resultse2rrt,abs(t)>2.4)[,c(5,1,2,3,4)]

## ----fig.width=11,fig.height=10------------------------------------------
plotresults(resultse2rrt,maintitle="Experiment 2")

## ------------------------------------------------------------------------
resultse2<-rbind(coeflmer(m1e2lrt), coeflmer(m2e2lrt), coeflmer(m3e2lrt),
      coeflmer(m4e2lrt), coeflmer(m5e2lrt), coeflmer(m6e2lrt), coeflmer(m7e2lrt))
contrasts<-rownames(resultse2)
contrasts<-factor(contrasts,levels=c("CL","BEI","CLxBEI"))

resultse2<-data.frame(resultse2)
colnames(resultse2)<-c("estimate","se","t")
resultse2<-cbind(contrasts,resultse2)

rois<-rep(c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"),each=3)
rois<-factor(rois,levels=c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))
resultse2<-cbind(resultse2,rois)

resultse2$lower<-resultse2$estimate-2*resultse2$se
resultse2$upper<-resultse2$estimate+2*resultse2$se
resultse2$expt<-"e2"

## ----fig.width=11,fig.height=10------------------------------------------
plotresults(resultse2,maintitle="Experiment 2",ylabel="Log RTs")

## ------------------------------------------------------------------------
resultse2<-rbind(coeflmer(m1e2lrt), coeflmer(m2e2lrt), coeflmer(m3e2lrt),
      coeflmer(m4e2lrt), coeflmer(m5e2lrt), coeflmer(m6e2lrt), coeflmer(m7e2lrt))
contrasts<-rownames(resultse2)
contrasts<-factor(contrasts,levels=c("CL","BEI","CLxBEI"))

resultse2<-data.frame(resultse2)
colnames(resultse2)<-c("estimate","se","t")
resultse2<-cbind(contrasts,resultse2)

rois<-rep(c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"),each=3)
rois<-factor(rois,levels=c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))
resultse2<-cbind(resultse2,rois)

resultse2$lower<-resultse2$estimate-2*resultse2$se
resultse2$upper<-resultse2$estimate+2*resultse2$se
resultse2$expt<-"e2"

## ----fig.width=11,fig.height=10------------------------------------------
plotresults(resultse2,maintitle="Experiment 2",ylabel="Log RTs")

## ------------------------------------------------------------------------
resultse2<-rbind(coeflmer(m1e2rt), coeflmer(m2e2rt), coeflmer(m3e2rt),
      coeflmer(m4e2rt), coeflmer(m5e2rt), coeflmer(m6e2rt), coeflmer(m7e2rt))
contrasts<-rownames(resultse2)
contrasts<-factor(contrasts,levels=c("CL","BEI","CLxBEI"))

resultse2<-data.frame(resultse2)
colnames(resultse2)<-c("estimate","se","t")
resultse2<-cbind(contrasts,resultse2)

rois<-rep(c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"),each=3)
rois<-factor(rois,levels=c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))
resultse2<-cbind(resultse2,rois)

resultse2$lower<-resultse2$estimate-2*resultse2$se
resultse2$upper<-resultse2$estimate+2*resultse2$se
resultse2$expt<-"e2"

## ----fig.width=11,fig.height=10------------------------------------------
plotresults(resultse2,maintitle="Experiment 2",ylabel="Raw RTs")

## ------------------------------------------------------------------------
e2crit<-subset(e2,
               roi%in%c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))

e2crit$roi<-as.factor(as.character(e2crit$roi))
unique(e2crit$roi)

e2crit$roi<-factor(e2crit$roi,
                   levels=c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))

M.id.w<-regionmeans(e2crit)
#M.id.w[,c(1,2,6,7,3,4)]

## ----fig.width=8,fig.height=8--------------------------------------------
M.id.w$condition<-factor(paste(M.id.w$condition,M.id.w$classifier,M.id.w$bei,sep=" "))

levels(M.id.w$condition)<-c("a NoCL NoBEI", "b CL NoBEI", "c NoCL BEI", "d CL BEI")

regionplot(M.id.w,maintitle="Experiment 2",legendpos=c(0.05,0.4))

## ----results="asis"------------------------------------------------------
print(xtable(M.id.w[,c(1,2,6,7,3,4)],digits=c(0,0,0,0,0,0,0)),
      type="html")

## ----fig.width=10,fig.height=15------------------------------------------
multiplot(plotresults(resultse2rrt,maintitle="Experiment 2"),
          regionplot(M.id.w,maintitle="Experiment 2",ymax=1200),ncol=1)
#,legendpos=c(0,0.4)),ncol=1)

## ----plotresultse1e2,fig.width=10,fig.height=12--------------------------
multiplot(plotresults(resultse1,maintitle="Experiment 1"),
          plotresults(resultse2,maintitle="Experiment 2"),ncol=1)

## ------------------------------------------------------------------------
sidak_alpha<-1-(1-0.05)^(1/7)
(crit_t_sidak<-round(abs(qnorm(sidak_alpha)),1))
bonf_alpha<-0.05/7
(crit_t_bonf<-round(abs(qnorm(bonf_alpha)),1))

## ----printresults,echo=FALSE,eval=TRUE,cache=FALSE,results="asis"--------
print(xtable(subset(resultse1,abs(t)>crit_t_sidak)),type="html")
print(xtable(subset(resultse2,abs(t)>crit_t_sidak)),type="html")

## ----combinedanalysis----------------------------------------------------

colnames(e1)
colnames(e1)[4]<-"rt"
colnames(e2)

e1e2<-rbind(e1,e2)

e1e2$expt<-ifelse(e1e2$expt=="e1",-1,1)

## RCSubj
m1<-lmer(rrt~(CL+BEI+CLxBEI)*expt+
           (1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e1e2,roi=="RCSubj"))
summary(m1)

qqPlot(residuals(m1))

##RCV
m2<-lmer(rrt~(CL+BEI+CLxBEI)*expt+(1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),subset(e1e2,roi=="RCV"))
summary(m2)

qqPlot(residuals(m2))

##de main effects and interactions
m3<-lmer(rrt~(CL+BEI+CLxBEI)*expt+(1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e1e2,roi=="de"))
summary(m3)

##headnoun
m4<-lmer(rrt~(CL+BEI+CLxBEI)*expt+(1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e1e2,roi=="headnoun"))
summary(m4)

qqPlot(residuals(m4))

##Adv main effects
m5<-lmer(rrt~(CL+BEI+CLxBEI)*expt+(1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e1e2,roi=="Adv"))
summary(m5)

qqPlot(residuals(m5))

##MainV main effects
m6<-lmer(rrt~(CL+BEI+CLxBEI)*expt+
           (1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e1e2,roi=="MainV"))
summary(m6)

#qqPlot(residuals(m6))

##MainO main effects CL effect
m7<-lmer(rrt~(CL+BEI+CLxBEI)*expt+(1+CL+BEI+CLxBEI||subject)+
           (1+CL+BEI+CLxBEI||item),
         subset(e1e2,roi=="MainO"))
summary(m7)

## ------------------------------------------------------------------------
resultse1e2<-rbind(coeflmer(m1), coeflmer(m2), coeflmer(m3),
      coeflmer(m4), coeflmer(m5), coeflmer(m6), coeflmer(m7))
contrasts<-rownames(resultse1e2)
contrasts<-factor(contrasts,levels=c("CL","BEI","CLxBEI"))

resultse1e2<-data.frame(resultse1e2)
colnames(resultse1e2)<-c("estimate","se","t")
resultse1e2<-cbind(contrasts,resultse1e2)

rois<-rep(c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"),each=3)
rois<-factor(rois,levels=c("RCSubj","RCV","de","headnoun","Adv","MainV","MainO"))
resultse1e2<-cbind(resultse1e2,rois)

resultse1e2$lower<-resultse1e2$estimate-2*resultse1e2$se
resultse1e2$upper<-resultse1e2$estimate+2*resultse1e2$se
resultse1e2$expt<-"e1e2"

## ----plotallresults,fig.width=8,fig.height=8-----------------------------
multiplot(plotresults(resultse1,maintitle="Experiment 1"),
          plotresults(resultse2,maintitle="Experiment 2"),
          plotresults(resultse1e2,maintitle="Experiments 1 and 2 combined"),ncol=1)


## ----processRmd, echo=FALSE, eval=FALSE----------------------------------
## ## not run:
## rmarkdown::render('./WuEtAlCogSci2016-vignette.Rmd')

