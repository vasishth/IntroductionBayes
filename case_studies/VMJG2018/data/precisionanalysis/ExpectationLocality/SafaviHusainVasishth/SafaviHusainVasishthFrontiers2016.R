## ----setup,cache=FALSE,include=FALSE-------------------------------------
# global chunk options
opts_chunk$set(cache=TRUE, autodep=TRUE,fig.path='./SafaviEtAlfigure', fig.align='center', fig.show='hold', cache.path='cache/graphics-',fig.height=5)
opts_knit$set(self.contained=FALSE)

#for labeling
knit_hooks$set(rexample = function(before, options, envir) {
  if (before) sprintf('\\begin{rexample}\\label{%s}\\hfill{}', options$label) 
    else '\\end{rexample}'
})
    

library(knitr)
require(lme4)
require(gridExtra)

## ----loadlibraries,echo=FALSE,include=FALSE------------------------------
library(lme4)
library(RePsychLing)
library(car)
library(reshape)
library(ggplot2)
library(plyr)
library(grid)
library(lattice)
library(xtable)
library(rstanarm)

## ----sentcomp,echo=FALSE-------------------------------------------------
## load sentence completion data:
d<-read.table('Pretests/entropy/sentcomp.txt', header=T)
## Expt 1:
exp1<-subset(d, Pre.test==1)
## three subjects incorrectly labeled twice with same ID:
## fix this:
## Fix problem by pasting list no. to make subjects unique:
exp1$subject<-factor(paste(exp1$subj.ID,exp1$LatSq.list,sep=""))
#unique(exp1$subject)

## sanity check:
#xtabs(~subject+cond,exp1)
#dim(subset(exp1,cond%in%c("a","b")))
#xtabs(~subject+item.ID,exp1)

exp2<-subset(d, Pre.test==2)
#xtabs(~subj.ID+cond,exp2)
exp2$subject<-factor(paste(exp2$subj.ID,exp2$LatSq.list,sep=""))
#unique(exp2$subject)

## compute accuracy of producing target verb:
## target verbs in sentence completion study,
## these are the verbs subjects should produce:
target_verbs_ab<-c("kardan","zadan",
                "kardan","kardan",
                "kardan","kardan",
                "kardan","dashtan",
                "bordan","kardan",
                "zadan","zadan",
                "zadan","zadan",
                "zadan","zadan",
                "kardan","kardan",
                "kardan","kardan",
                "dadan","kardan",
                "dadan","dadan",
                "goftan","kardan",
                "kardan","kardan",
                "gozashtan","zadan",
                "kardan","kardan",
                "kardan","kardan",
                "gereftan","kardan")
             
target_verbs_cd<-c("pokhtan", "kharidan", 
                   "dukhtan", "shenidan", 
                   "shenidan", "neveshtan", 
                   "khandan", "dashtan", 
                   "dashtan", "gereftan", 
                   "avikhtan", "goftan", 
                   "forukhtan", "dadan", 
                   "avardan", "dadan", 
                   "amukhtan", "fahmandan", 
                   "bakhshidan", "dadan", 
                   "resandan", "sepordan", 
                   "ferestadan", "dadan", 
                   "dadan", "khandan", 
                   "goftan", "navakhtan", 
                   "raftan", "shenidan", 
                   "shenidan", "fahmidan", 
                   "andishidan", "fahmidan", 
                   "fahmidan", "neveshtan")   

## data frame with target verbs:
targets_ab_df<-data.frame(item=rep(1:36,each=2),
                    cond=rep(letters[1:2],36),
                    target_verbs=rep(target_verbs_ab,each=2))
targets_cd_df<-data.frame(item=rep(1:36,each=2),
                    cond=rep(letters[3:4],36),
                    target_verbs=rep(target_verbs_cd,each=2))
targets<-rbind(targets_ab_df,targets_cd_df)

## Exp 1:
nrows<-dim(exp1)[1]
## Next, create a column marking "correct completion"
hit<-rep(NA,nrows)

## identify exact matches with target:
for(i in 1:nrows){
  ## get i-th row:
  tmp<-exp1[i,]
  itemid<-tmp$item.ID
  condition<-as.character(tmp$cond)
  tmpverb<-as.character(tmp$verb)
  ## find appropriate row in targets data frame:
  target_row<-subset(targets,item==itemid & 
                       cond==condition)
  targetverb<-as.character(target_row$target_verbs)
  if(targetverb==tmpverb){
    hit[i]<-1} else {hit[i]<-0}
  }

## 1 if target verb produced, 0 otherwise
exp1$target<-hit

## Exp 2:
## Next, create a column marking "correct completion"
nrows<-dim(exp2)[1]
hit<-rep(NA,nrows)

for(i in 1:nrows){
  ## get i-th row:
  tmp<-exp2[i,]
  itemid<-tmp$item.ID
  condition<-as.character(tmp$cond)
  tmpverb<-as.character(tmp$verb)
  ## find appropriate row in targets data frame:
  target_row<-subset(targets,item==itemid & 
                       cond==condition)
  targetverb<-as.character(target_row$target_verbs)
  if(targetverb==tmpverb){
    hit[i]<-1} else {hit[i]<-0}
  }

## 1 if target verb produced, 0 otherwise
exp2$target<-hit

## slight reduction in prob. of target with distance:
sentcomp_mn_exp1<-round(100*with(exp1,tapply(target,cond,mean)),digits=2)
sentcomp_mn_exp2<-round(100*with(exp2,tapply(target,cond,mean)),digits=2)

## contrast coding:
exp1$Predictability<-ifelse(exp1$cond%in%c("a","b"),1,-1)
exp1$Distance<-ifelse(exp1$cond%in%c("a","c"),-1,1)

exp2$Predictability<-ifelse(exp2$cond%in%c("a","b"),1,-1)
exp2$Distance<-ifelse(exp2$cond%in%c("a","c"),-1,1)

library(lme4)
## fails to converge:
#sentcomptarget<-glmer(target~predictability*distance+
#                 (1+predictability*distance||subject)+
#                 (1+predictability*distance||item.ID),
#               family=binomial(),
#               exp1)
#summary(sentcomptarget)

sentcomptargete1<-glmer(target~Distance*Predictability+
                 (1|subject)+
                 (1|item.ID),
               family=binomial(),
               exp1)

sentcomptargete2<-glmer(target~Distance*Predictability+
                 (1|subject)+
                 (1|item.ID),
               family=binomial(),
               exp2)

#summary(sentcomptargete1)
#summary(sentcomptargete2)

## Priors: plausible values of 
## prob range from 0.0001 to 0.9999
## Intercept: t(2) a bit like N(0,2.96^2)
## Slope: t(2) a bit like N(0,2.96^2)

library(rstanarm)

if(0){ ## avoiding running at compile time
sentcompstane1<-stan_glmer(target~Distance*Predictability+
                    (1+Distance*Predictability|subject)+
                    (1+Distance*Predictability|item.ID),
                  family=binomial(),
                  prior_intercept=student_t(df=2),
                  prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  exp1,
                  chains=4,
                  iter=2000,
                  cores=4)

sentcompstane2<-stan_glmer(target~Distance*Predictability+
                    (1+Distance*Predictability|subject)+
                    (1+Distance*Predictability|item.ID),
                  family=binomial(),
                  prior_intercept=student_t(df=2),
                  prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  exp2,
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=2000,
                  cores=4)

save(sentcompstane1,file="sentcompstane1.Rda")
save(sentcompstane2,file="sentcompstane2.Rda")
}

load("sentcompstane1.Rda")
load("sentcompstane2.Rda")

## function for extracting results:
summary_stan_model<-function(mod,nfixefs=3){
  samples_m1 <- as.data.frame(mod)
  nfixefs<-nfixefs+1 ## including intercept
  mns<-rep(NA,nfixefs)
  ci95<-matrix(rep(NA,nfixefs*2),ncol=2)
  for(i in 1:nfixefs){
  condnames<-colnames(samples_m1)[1:nfixefs]
  condnames[1]<-"Intercept"
  mns[i]<-round(mean(samples_m1[,i]),digits=4) 
  ci95[i,]<-round(quantile(probs=c(0.025,0.975),samples_m1[,i]),digits=4)
  }
  ## prob less than 0
  prob_less<-rep(NA,nfixefs)
  for(i in 1:nfixefs){
  prob_less[i]<-round(mean(samples_m1[,i]<0),digits=4)
  }
  res<-as.data.frame(cbind(condnames,mns,ci95,prob_less))
  colnames(res)<-c("comparison","mean","lower","upper","P(b<0)")
  return(res)
}

sce1_xtab<-summary_stan_model(sentcompstane1)
sce2_xtab<-summary_stan_model(sentcompstane2)

## ----exp1analysis,echo=FALSE---------------------------------------------
data1<-read.table("data_spr/Persiane1.txt", header=T)

data1$pos<-factor(data1$pos)
data1$resp<-factor(data1$resp)
data1$cond<-factor(data1$cond)

#questions (response accuracy)
data1.q <- subset(data1, pos=="?")
data1.q$resp<- as.numeric(
  as.character(data1.q$resp))
meansq<-round(100*with(data1.q,
                       tapply(resp,cond,mean)),
              digits=1)

data1.q$dist<-ifelse(data1.q$cond%in%c("a","c"),-1,1)
## pred 1, -1
data1.q$pred<-ifelse(data1.q$cond%in%c("a","b"),1,-1)

m1glmer<-glmer(resp~dist*pred+(1|subj)+(1|item),family=binomial(),data1.q)
#summary(m1glmer)

if(0){
stanglmerm1<-stan_glmer(resp~dist*pred+(1+dist+pred|subj)+
               (1+dist+pred|item),
               family=binomial(),
                     prior_intercept=student_t(df=2),
                     prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=data1.q)

save(stanglmerm1,file="stanglmerm1.Rda")
}


load("stanglmerm1.Rda")
stanglmerm1_tab<-summary_stan_model(stanglmerm1)

## Reading times analysis:
## short -1, long 1
data1$dist<-ifelse(data1$cond%in%c("a","c"),-1,1)
## pred 1, -1
data1$pred<-ifelse(data1$cond%in%c("a","b"),1,-1)

#nested
data1$pred.dist <- ifelse(data1$cond=="a",-1,
                          ifelse(data1$cond=="b",1,0))
data1$nopred.dist <- ifelse(data1$cond=="c",-1,
                         ifelse(data1$cond=="d",1,0))

## 4 extreme data points removed: 0.02%

m1<-lmer(log(rt)~dist*pred+(1+dist+dist:pred||subj)+(1+pred||item),subset(data1,roi=="crit" & rt<3000))
#summary(m1)

if(0){
stanm1<-stan_lmer(log(rt)~dist*pred+(1+dist*pred|subj)+
               (1+dist*pred|item),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.99,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=subset(data1,roi=="crit" & rt<3000))

save(stanm1,file="stanm1.Rda")

stanm1nested<-stan_lmer(log(rt)~pred+pred.dist+nopred.dist+(1+pred+pred.dist+nopred.dist|subj)+
               (1+pred+pred.dist+nopred.dist|item),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.99,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=subset(data1,roi=="crit" & rt<3000))

save(stanm1nested,file="stanm1nested.Rda")
}

load("stanm1.Rda")
load("stanm1nested.Rda")

stanm1_tab<-summary_stan_model(stanm1)
stanm1nested_tab<-summary_stan_model(stanm1nested)

## create e1crit for comparison with exp2:
e1crit<-subset(data1,roi="crit")

## ----plotexpt1,echo=FALSE------------------------------------------------
data1 <- subset(data1, pos!='?')

a.regions<-c("predep","dep","precrit","crit","postcrit","post")
b.regions<-c("predep","dep","precrit","crit","postcrit","post")
c.regions<-c("predep","dep","precrit","crit","postcrit","post")
d.regions<-c("predep","dep","precrit","crit","postcrit","post")

regions<-c(a.regions,b.regions,c.regions,d.regions)
region.id<-rep(1:6,4)
cond.id<-rep(letters[1:4],each=6)

region.df<-data.frame(cond=factor(cond.id),
                      region.id=region.id,
                      roi=factor(regions))

#merge multiple regions with same id
data1.uniq.reg<-ddply(data1, 
                     .(subj, item, cond, roi),
                     summarize, 
                     rt = sum(rt))

data1.merged<-merge(data1.uniq.reg,region.df, 
                   by.x=c("cond","roi"))
data1.merged$cond<-droplevels(data1.merged$cond)

Expectation<-factor(ifelse(data1.merged$cond%in%c("a","b"),"strong-exp","weak-exp"),
             levels=c("strong-exp","weak-exp"))
dist<-factor(ifelse(data1.merged$cond%in%c("a","c"),"short","long"),
                levels=c("short","long"))

data1.merged$Expectation<-Expectation
data1.merged$dist<-dist

data.rs <- melt(data1.merged, 
                id=c("Expectation","dist","cond","roi",
                     "region.id","subj"), 
                measure=c("rt"),
                na.rm=TRUE)


#get mean rt and no. of data points for each region, for each subject/condition
data.id  <- data.frame(cast(data.rs, 
                            subj+Expectation+dist+cond+roi+region.id ~ ., 
                            function(x) c(rt=mean(x), N=length(x) ) ))

#mean of mean rt of each subject
GM <- mean(tapply(data.id$rt, data.id$subj, mean))

#deviation from the grandmean after considering intra-subject variability
#removing between subject variance
data.id <- ddply(data.id, .(subj), 
                 transform, rt.w = rt - mean(rt) + GM)

temp<-melt(data.id, id.var=c("subj","Expectation","dist","cond","roi","region.id"), 
           measure.var="rt.w")

M.id.w <- cast(temp, Expectation+dist+cond+roi+region.id  ~ ., 
               function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )
M.id.w.pred <- cast(temp, Expectation+roi+region.id  ~ ., 
                    function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

k<-1

#head(M.id.w)
M.id.w.orig<-M.id.w
## make names consistent with paper:
colnames(M.id.w)[1]<-"Predictability"
M.id.w$Predictability<-factor(ifelse(M.id.w$Predictability=="strong-exp","strong","weak"))

p1a<-ggplot(subset(M.id.w,roi=="crit"), 
             aes(x=dist, y=M,group=Predictability)) + 
               geom_point(shape=21,fill="white",size=k*3) +
               geom_line(aes(linetype=Predictability),size=k) +
               geom_errorbar(aes(ymin=M-2*SE, ymax=M+2*SE),
                             width=.1,size=k)+
                               xlab("Distance")+
                               ylab("reading time [RT in ms]")+                                               labs(title="Critical region [Verb]") + theme_bw() + labs(legend.position=c(.87, .6))

p1a+theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.text = element_text(colour="black", size = 16, face = "bold"))+
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"))

## ----exp2analysis,echo=FALSE---------------------------------------------
data2<-read.table("data_spr/persiane2.txt", header=T)

data2$pos<-factor(data2$pos)
data2$resp<-factor(data2$resp)
data2$cond<-factor(data2$cond)

## Question response accuracy
data2.q <- subset(data2, pos=="?")
data2.q$resp<- as.numeric(as.character(data2.q$resp))
meansq<-round(100*with(data2.q,tapply(resp,cond,mean)),digits=1)

###accuracy analysis
data2.q$dist<-ifelse(data2.q$cond%in%c("a","c"),-1,1)
## pred 1, -1
data2.q$pred<-ifelse(data2.q$cond%in%c("a","b"),1,-1)

#m2glmer<-glmer(resp~dist*pred+(1|subj)+(1|item),family=binomial(),data2.q)
#summary(m2glmer)
## cf:
#m1glmer<-glmer(resp~dist*pred+(1+dist*pred||subj)+(1|item),family=binomial(),data1.q)
#summary(m1glmer)

if(0){
stanglmerm2<-stan_glmer(resp~dist*pred+(1+dist+pred|subj)+
               (1+dist+pred|item),
               family=binomial(),
                     prior_intercept=student_t(df=2),
                     prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=data2.q)

save(stanglmerm2,file="stanglmerm2.Rda")
}

load("stanglmerm2.Rda")
stanglmerm2_tab<-summary_stan_model(stanglmerm2)

## short -1, long 1
data2$dist<-ifelse(data2$cond%in%c("a","c"),-1,1)
## pred 1, -1
data2$pred<-ifelse(data2$cond%in%c("a","b"),1,-1)

#nested
data2$pred.dist <- ifelse(data2$cond=="a",-1,
                          ifelse(data2$cond=="b",1,0))
data2$nopred.dist <- ifelse(data2$cond=="c",-1,
                            ifelse(data2$cond=="d",1,0))

## 6 extreme data points removed: 0.4%
#m2<-lmer(log(rt)~dist*pred+(1+dist+dist:pred||subj)+(1+dist+dist:pred||item),subset(data2,roi=="crit"))

#m2<-lmer(log(rt)~dist*pred+(1+dist+dist:pred||subj)+(1+dist+dist:pred||item),subset(data2,roi=="crit" & rt<3000))
#summary(m2)
#qqPlot(residuals(m2))

#Nothing much changes if we remove items 5, 9, 26, 32, which had low values:

#data2subset<-subset(data2,item!=5 & item!=9 & item!=26 & item!=32)
#length(sort(unique(data2subset$item)))

#m2a<-lmer(log(rt)~dist*pred+(1+dist+dist:pred||subj)+(1+dist+dist:pred||item),subset(data2subset,roi=="crit" & rt<3000))
#summary(m2)

#m2.nested<-lmer(log(rt)~pred+pred.dist+nopred.dist+(1+pred.dist+nopred.dist||subj)+(1+pred.dist+nopred.dist||item),subset(data2,roi=="crit" & rt<3000))
#summary(m2.nested) 

## main and nested analysis
if(0){
stanm2<-stan_lmer(log(rt)~dist*pred+(1+dist*pred|subj)+
               (1+dist*pred|item),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.99,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=subset(data2,roi=="crit" & rt<3000))

save(stanm2,file="stanm2.Rda")

stanm2nested<-stan_lmer(log(rt)~pred+pred.dist+nopred.dist+(1+pred+pred.dist+nopred.dist|subj)+
               (1+pred+pred.dist+nopred.dist|item),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=subset(data2,roi=="crit" & rt<3000))

save(stanm2nested,file="stanm2nested.Rda")
}

load("stanm2.Rda")
load("stanm2nested.Rda")

stanm2_tab<-summary_stan_model(stanm2)
stanm2nested_tab<-summary_stan_model(stanm2nested)

#Extract data from critical region for comparing with expt 1:

e2crit<-subset(data2,roi=="crit")
e1crit$expt<-factor("e1")
e1crit$subj<-paste("e1",e1crit$subj,sep="")
e2crit$expt<-factor("e2")
e2crit$subj<-paste("e2",e2crit$subj,sep="")
e1e2crit<-rbind(e1crit,e2crit)
e1e2crit$expt<-ifelse(e1e2crit$expt=="e1",-1,1)
e1e2crit$subj<-factor(e1e2crit$subj)

## Is there an interaction of expt with distance and pred: yes
me1e2<-lmer(log(rt)~ dist*pred+expt + dist:expt + pred:expt + dist:pred:expt + (1+dist*pred||subj) + (1+dist*pred||item),
            subset(e1e2crit,roi=="crit" & rt<3000))
#round(summary(me1e2)$coefficients,digits=2)

if(0){
stanm2combined<-stan_lmer(log(rt)~dist*pred+expt + dist:expt + pred:expt + dist:pred:expt+(1+dist*pred|subj)+
               (1+dist*pred|item),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.99,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=subset(e1e2crit,roi=="crit" & rt<3000))

#print(stanm2combined)

save(stanm2combined,file="stanm2combined.Rda")

}
load("stanm2combined.Rda")

stanm2combined_tab<-summary_stan_model(stanm2combined,nfixefs=7)

## ----plotexpt2,echo=FALSE------------------------------------------------
data2 <- subset(data2, pos!='?')

a.regions<-c("predep","dep","precrit","crit","postcrit","post")
b.regions<-c("predep","dep","precrit","crit","postcrit","post")
c.regions<-c("predep","dep","precrit","crit","postcrit","post")
d.regions<-c("predep","dep","precrit","crit","postcrit","post")

regions<-c(a.regions,b.regions,c.regions,d.regions)
region.id<-rep(1:6,4)
cond.id<-rep(letters[1:4],each=6)

region.df<-data.frame(cond=factor(cond.id),
                      region.id=region.id,
                      roi=factor(regions))

#merge multiple regions with same id
data2.uniq.reg<-ddply(data2, 
                      .(subj, item, cond, roi),
                      summarize, 
                      rt = sum(rt))

data2.merged<-merge(data2.uniq.reg,region.df, 
                    by.x=c("cond","roi"))
data2.merged$cond<-droplevels(data2.merged$cond)

Expectation<-factor(ifelse(data2.merged$cond%in%c("a","b"),"strong-exp","weak-exp"),
             levels=c("strong-exp","weak-exp"))
dist<-factor(ifelse(data2.merged$cond%in%c("a","c"),"short","long"),
             levels=c("short","long"))

data2.merged$Expectation<-Expectation
data2.merged$dist<-dist

data.rs <- melt(data2.merged, 
                id=c("Expectation","dist","cond","roi",
                     "region.id","subj"), 
                measure=c("rt"),
                na.rm=TRUE)


#get mean rt and no. of data points for each region, for each subject/condition
data.id  <- data.frame(cast(data.rs, 
                            subj+Expectation+dist+cond+roi+region.id ~ ., 
                            function(x) c(rt=mean(x), N=length(x) ) ))

#mean of mean rt of each subject
GM <- mean(tapply(data.id$rt, data.id$subj, mean))

#deviation from the grandmean after considering intra-subject variability
#removing between subject variance
data.id <- ddply(data.id, .(subj), 
                 transform, rt.w = rt - mean(rt) + GM)

temp<-melt(data.id, id.var=c("subj","Expectation","dist","cond","roi","region.id"), 
           measure.var="rt.w")

M.id.w <- cast(temp, Expectation+dist+cond+roi+region.id  ~ ., 
               function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )
M.id.w.pred <- cast(temp, Expectation+roi+region.id  ~ ., 
                    function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

k<-1

M.id.w.orig<-M.id.w
## make names consistent with paper:
colnames(M.id.w)[1]<-"Predictability"
M.id.w$Predictability<-factor(ifelse(M.id.w$Predictability=="strong-exp","strong","weak"))

p2a<-ggplot(subset(M.id.w,roi=="crit"), 
             aes(x=dist, y=M,group=Predictability)) + 
   geom_point(shape=21,fill="white",size=k*3) +
   geom_line(aes(linetype=Predictability),size=k) +
   geom_errorbar(aes(ymin=M-2*SE, ymax=M+2*SE),
                 width=.1,size=k)+
   xlab("Distance")+
   ylab("reading time [RT in ms]")+                     
   labs(title="Critical region [Verb]") +
   theme_bw() +
   labs(legend.position=c(.87, .6))

p2a+theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 16, face = "bold"))+
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"))

## ----byregionplotexcluded,echo=FALSE,eval=FALSE--------------------------
## M.id.w <- M.id.w.orig
## 
## #############
## 
## M.id.w.cp<-subset(M.id.w,Expectation=="strong-exp")
## M.id.w.cp$Expectation<-droplevels(M.id.w.cp$Expectation)
## 
## M.id.w.sp<-subset(M.id.w,Expectation=="weak-exp")
## M.id.w.sp$Expectation<-droplevels(M.id.w.sp$Expectation)
## 
## ## function for generic by-region plots (effect of distance within CP vs. SP):
## byregion.plot<-function(data,
##                         mytitle,k=1,
##                         x.lab="position",
##                         y.lab="reading time [Raw rt]"){
##   ggplot(data,aes(x=region.id,y=M,
##                   group=dist)) +
##     geom_point(shape=21,size=k*2) +
##     geom_line(aes(linetype=dist),size=k) +
##     geom_errorbar(aes(ymin=M-2*SE,
##                       ymax=M+2*SE),
##                   width=.1,size=k)+
##     #xlab(x.lab)+
##     scale_x_continuous(x.lab, breaks = 1:6, labels = c("predep","dep","precrit","crit","postcrit","post"))+
##     #ylab(y.lab)+
##     #opts(title=mytitle) +
##     #theme_bw()+
##     #theme(axis.text.x = element_text(size=7))
##     labs(x ='\nPosition', y = 'Reading time [msec]\n')+
##     theme_bw()+
##     ggtitle(mytitle)}
## 
## (plot.cp<-byregion.plot(M.id.w.cp,
##                         mytitle="Effect of distance [strong expectation]",k=1,
##                         x.lab="position",y.lab="reading time [Raw rt]")
## )
## 
## 
## (plot.sp<-byregion.plot(M.id.w.sp,
##                         mytitle="Effect of distance [weak expectation]",k=1,
##                         x.lab="position",y.lab="reading time [log ms]")
## )
## 
## 
## M.id.w.short<-subset(M.id.w,dist=="short")
## M.id.w.short$dist <- droplevels(M.id.w.short$dist)
## 
## M.id.w.long<-subset(M.id.w,dist=="long")
## M.id.w.long$dist <- droplevels(M.id.w.long$dist)
## 
## byregion.plot<-function(data,
##                         mytitle,k=1,
##                         x.lab="position",
##                         y.lab="reading time [msec]"){
##   ggplot(data,aes(x=region.id,y=M,
##                   group=Expectation)) +
##     geom_point(shape=21,size=k*2) +
##     geom_line(aes(linetype=Expectation),size=k) +
##     geom_errorbar(aes(ymin=M-2*SE,
##                       ymax=M+2*SE),
##                   width=.1,size=k)+
##     #xlab(x.lab)+
##     scale_x_continuous(x.lab, breaks = 1:6, labels = c("predep","dep","precrit","crit","postcrit","post"))+
##     #ylab(y.lab)+
##     #opts(title=mytitle) +
##     #theme_bw()+
##     #theme(axis.text.x = element_text(size=7))
##     labs(x ='\nPosition', y = 'Reading time [msec]\n')+
##     theme_bw()+
##     ggtitle(mytitle)}
## 
## 
## (byregion.plot(M.id.w.long,
##                mytitle="Effect of expectation (long conditions)",k=1,
##                x.lab="position",y.lab="reading time [msec]")
## )
## 
## (byregion.plot(M.id.w.short,
##                mytitle="Effect of expectation (short conditions)",k=1,
##                x.lab="position",y.lab="reading time [msec]")
## )

## ----exp3analysis,echo=FALSE---------------------------------------------
data1<-read.table("ET/ET1-final.txt", header=T)

# details of data 1 :
#head(data1)
#summary(data1)
#str(data1)
#length(unique(data1$subject))   # 40
#length(unique(data1$id))   # 32

data1$subject<-factor(data1$subject)
#data1$RESPONSE_ACCURACY<-factor(data1$RESPONSE_ACCURACY)
data1$cond<-factor(data1$cond)

#contrast coding :
## short -1, long 1
data1$dist<-ifelse(data1$cond%in%c("a","c"),-1,1)
## pred 1, -1
data1$pred<-ifelse(data1$cond%in%c("a","b"),1,-1)

#nested
data1$pred.dist <- ifelse(data1$cond=="a",-1,
                          ifelse(data1$cond=="b",1,0))
data1$nopred.dist <- ifelse(data1$cond=="c",-1,
                            ifelse(data1$cond=="d",1,0))

#Define the position column:

data1$position<-NA

# crit :
data1$position[data1$cond=="a" & data1$roi==4] <- "crit"
data1$position[data1$cond=="b" & data1$roi==5]<- "crit"
data1$position[data1$cond=="c" & data1$roi==4]<- "crit"
data1$position[data1$cond=="d" & data1$roi==5]<- "crit"

#preverb or object :
data1$position[data1$cond=="a" & data1$roi==2] <- "dep"
data1$position[data1$cond=="b" & data1$roi==2]<- "dep"
data1$position[data1$cond=="c" & data1$roi==2]<- "dep"
data1$position[data1$cond=="d" & data1$roi==2]<- "dep"

# subject :
data1$position[data1$cond=="a" & data1$roi==1] <- "predep"
data1$position[data1$cond=="b" & data1$roi==1]<- "predep"
data1$position[data1$cond=="c" & data1$roi==1]<- "predep"
data1$position[data1$cond=="d" & data1$roi==1]<- "predep"

# intervener :
data1$position[data1$cond=="a" & data1$roi==3] <- "intervener"
data1$position[data1$cond=="b" & data1$roi==3]<- "intervener"
data1$position[data1$cond=="b" & data1$roi==4]<- "intervener"
data1$position[data1$cond=="c" & data1$roi==3]<- "intervener"
data1$position[data1$cond=="d" & data1$roi==3]<- "intervener"
data1$position[data1$cond=="d" & data1$roi==4]<- "intervener"

#post crit
data1$position[data1$cond=="a" & data1$roi==5] <- "postcrit"
data1$position[data1$cond=="b" & data1$roi==6]<- "postcrit"
data1$position[data1$cond=="c" & data1$roi==5]<- "postcrit"
data1$position[data1$cond=="d" & data1$roi==6]<- "postcrit"

#post :
data1$position[data1$cond=="a" & data1$roi==6] <- "post"
data1$position[data1$cond=="b" & data1$roi==7]<- "post"
data1$position[data1$cond=="c" & data1$roi==6]<- "post"
data1$position[data1$cond=="d" & data1$roi==7]<- "post"

#Looking at the proportions of 0 fixations:
## counts of 0 ms fixations:
#nrow(data1[data1$FFD == 0&data1$cond=='a',])
#nrow(data1[data1$FFD == 0&data1$cond=='b',])
#nrow(data1[data1$FFD == 0&data1$cond=='c',])
#nrow(data1[data1$FFD == 0&data1$cond=='d',])

## 0 ms fixations:
#nrow(data1[data1$FFD == 0&data1$cond=='a',])/nrow(data1)
#nrow(data1[data1$FFD == 0&data1$cond=='b',])/nrow(data1)
#nrow(data1[data1$FFD == 0&data1$cond=='c',])/nrow(data1)
#nrow(data1[data1$FFD == 0&data1$cond=='d',])/nrow(data1)
#nrow(data1[data1$FFD == 0,])/nrow(data1)

#Data analysis of FPRT, RPD:

#contrast coding :
## short -1, long 1
data1$dist<-ifelse(data1$cond%in%c("a","c"),-1,1)
## pred 1, -1
data1$pred<-ifelse(data1$cond%in%c("a","b"),1,-1)

#nested
data1$pred.dist <- ifelse(data1$cond=="a",-1,
                          ifelse(data1$cond=="b",1,0))
data1$nopred.dist <- ifelse(data1$cond=="c",-1,
                            ifelse(data1$cond=="d",1,0))

## needed later for entropy analysis
etdata1<-data1

ET1.FPRT<-lmer(log(FPRT)~dist*pred+(1+dist*pred|subject)+(1+dist*pred|id),subset(data1,position=="crit" & FPRT!=0))
#summary(ET1.FPRT)
#summary(rePCA(ET1.FPRT))

## subj and obj top 3
#ET1.FPRTfin<-lmer(log(FPRT)~dist*pred+(1+dist+pred||subject)+(1+dist+pred||id),subset(data1,position=="crit" & FPRT!=0))
#summary(ET1.FPRTfin)
#qqPlot(residuals(ET1.FPRTfin))

#ET1.RPD<-lmer(log(RPD)~dist*pred+(1+dist*pred||subject)+(1+dist*pred||id),subset(data1,position=="crit" & RPD!=0))
#summary(ET1.RPD)
#summary(rePCA(ET1.RPD))
## subj and obj top 3
#ET1.RPDfin<-lmer(log(RPD)~dist*pred+(1+dist+pred||subject)+(1+dist+pred||id),subset(data1,position=="crit" & RPD!=0))
#summary(ET1.RPDfin)
#qqPlot(residuals(ET1.RPDfin))

if(0){
stanm3fprt<-stan_lmer(log(FPRT)~dist*pred+
                        (1+dist*pred|subject)+
               (1+dist*pred|id),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.999,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=subset(data1,position=="crit" & FPRT!=0))

save(stanm3fprt,file="stanm3fprt.Rda")

stanm3nestedfprt<-stan_lmer(log(FPRT)~pred+pred.dist+nopred.dist+(1+pred+pred.dist+nopred.dist|subject)+
               (1+pred+pred.dist+nopred.dist|id),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.999,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=subset(data1,position=="crit" & FPRT!=0))

save(stanm3nestedfprt,file="stanm3nestedfprt.Rda")

## RPD:
stanm3rpd<-stan_lmer(log(RPD)~dist*pred+
                        (1+dist*pred|subject)+
               (1+dist*pred|id),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=3000,
                  cores=4,
                  data=subset(data1,position=="crit" & RPD!=0))

save(stanm3rpd,file="stanm3rpd.Rda")

stanm3nestedrpd<-stan_lmer(log(RPD)~pred+pred.dist+nopred.dist+(1+pred+pred.dist+nopred.dist|subject)+
               (1+pred+pred.dist+nopred.dist|id),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=4000,
                  cores=4,
                  data=subset(data1,position=="crit" & RPD!=0))

save(stanm3nestedrpd,file="stanm3nestedrpd.Rda")

}

load("stanm3fprt.Rda")
load("stanm3nestedfprt.Rda")

load("stanm3rpd.Rda")
load("stanm3nestedrpd.Rda")

stanm3fprt_tab<-summary_stan_model(stanm3fprt)
stanm3nestedfprt_tab<-summary_stan_model(stanm3nestedfprt)

stanm3rpd_tab<-summary_stan_model(stanm3rpd)
stanm3nestedrpd_tab<-summary_stan_model(stanm3nestedrpd)

## ----exp3questionresponseanalysis,echo=FALSE-----------------------------
etdata1q<-read.table("ET/etdata1qrespacc.txt",header=TRUE)
#head(etdata1q)

meanset1q<-round(with(etdata1q,tapply(resp,cond,mean))*100)

if(0){
stanglmeretqm1<-stan_glmer(resp~dist*pred+(1+dist+pred|subject)+
               (1+dist+pred|id),
               family=binomial(),
                     prior_intercept=student_t(df=2),
                     prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=3000,
                  cores=4,
                  data=etdata1q)

stanglmeretqm1nested<-stan_glmer(resp~pred+pred.dist+nopred.dist
                                 +(1+pred+pred.dist+nopred.dist|subject)+
               (1+pred+pred.dist+nopred.dist|id),
               family=binomial(),
                     prior_intercept=student_t(df=2),
                     prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=etdata1q)

save(stanglmeretqm1,file="stanglmeretqm1.Rda")

save(stanglmeretqm1nested,file="stanglmeretqm1nested.Rda")
}


load("stanglmeretqm1.Rda")
stanglmeretqm1_tab<-summary_stan_model(stanglmeretqm1)

load("stanglmeretqm1nested.Rda")
stanglmeretqm1nested_tab<-summary_stan_model(stanglmeretqm1nested)

## ----plotsET1,echo=FALSE-------------------------------------------------
data1 <- na.omit(data1) 

data1<-subset(data1,FPRT!=0 & RPD!=0)
a.regions<-c("predep","dep","intervener","crit","postcrit", "post")
b.regions<-c("predep","dep","intervener","crit","postcrit", "post")
c.regions<-c("predep","dep","intervener","crit","postcrit", "post")
d.regions<-c("predep","dep","intervener","crit","postcrit", "post")

regions<-c(a.regions,b.regions,c.regions,d.regions)
region.id<-rep(1:6,4)
cond.id<-rep(letters[1:4],each=6)

region.df<-data.frame(cond=factor(cond.id),
                      region.id=region.id,
                      position=factor(regions))

data1.uniq.reg<-ddply(data1, 
                      .(subject, id, cond, position),
                      summarize,
                      FPRT = FPRT,
                      RPD = RPD)

data1.merged<-merge(data1.uniq.reg,region.df, 
                    by.x=c("cond","position"))
#summary(data1.merged)

data1 <- data1.merged

pred<-factor(ifelse(data1$cond%in%c("a","b"),
                    "pred","nopred"),
             levels=c("pred","nopred"))
dist<-factor(ifelse(data1$cond%in%c("a","c"),
                    "short","long"),
             levels=c("short","long"))

data1$pred<-pred
data1$dist<-dist

dataFPRT.rs <- melt(data1, 
                id=c("pred","dist","cond","position",
                     "subject","region.id"), 
                measure=c("FPRT"),
                na.rm=TRUE)

dataRPD.rs <- melt(data1, 
                id=c("pred","dist","cond","position",
                     "subject","region.id"), 
                measure=c("RPD"),
                na.rm=TRUE)

#dataTFT.rs <- melt(data1, 
#                id=c("pred","dist","cond","position",
#                     "subject","region.id"), 
#                measure=c("TFT"),
#                na.rm=TRUE)

## First-pass reading time

#get mean TFT and no. of data points for each region, for each subject/condition
dataFPRT.id <- data.frame(cast(dataFPRT.rs,
            subject+pred+dist+cond+position+region.id ~ ., 
                            function(x) c(FPRT=mean(x), N=length(x))))

#mean of mean rt of each subject
GM <- mean(tapply(dataFPRT.id$FPRT, dataFPRT.id$subject, mean))

#deviation from the grandmean after considering intra-subject variability
#removing between subject variance
dataFPRT.id <- ddply(dataFPRT.id, .(subject), 
                 transform, FPRT.w = FPRT - mean(FPRT) + GM)

temp<-melt(dataFPRT.id, id.var=c("subject","pred","dist","cond","position","region.id"), 
           measure.var="FPRT.w")

M.id.w <- cast(temp, pred+dist+cond+position+region.id  ~ ., 
               function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

M.id.w.pred <- cast(temp, pred+position  ~ ., 
                    function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

M.id.w.cp<-subset(M.id.w,pred=="pred")
M.id.w.cp$pred<-droplevels(M.id.w.cp$pred)

M.id.w.sp<-subset(M.id.w,pred=="nopred")
M.id.w.sp$pred<-droplevels(M.id.w.sp$pred)

M.id.w.orig<-M.id.w

colnames(M.id.w)[1]<-"Predictability"
M.id.w$Predictability<-factor(ifelse(M.id.w$Predictability=="pred","strong","weak"))


k<-1

plotfprt<-ggplot(subset(M.id.w,position=="crit"), 
             aes(x=dist, y=M,group=Predictability)) + 
   geom_point(shape=21,fill="white",size=k*3) +
   geom_line(aes(linetype=Predictability),size=k) +
   geom_errorbar(aes(ymin=M-2*SE, ymax=M+2*SE),
                 width=.1,size=k)+
   xlab("Distance")+
   ylab("FPRT (ms)")+
   labs(title="FPRT at the Critical region [Verb]") +
   theme_bw() +
   labs(legend.position=c(.87, .6))

plotfprt<-plotfprt+theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 16, face = "bold"))+
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"))

M.id.w <- M.id.w.orig

## Regression path duration
#get mean TFT and no. of data points for each region, for each subject/condition
dataRPD.id <- data.frame(cast(dataRPD.rs,
            subject+pred+dist+cond+position+region.id ~ ., 
                            function(x) c(RPD=mean(x), N=length(x))))

#mean of mean rt of each subject
GM <- mean(tapply(dataRPD.id$RPD, dataRPD.id$subject, mean))

#deviation from the grandmean after considering intra-subject variability
#removing between subject variance
dataRPD.id <- ddply(dataRPD.id, .(subject), 
                 transform, RPD.w = RPD - mean(RPD) + GM)

temp<-melt(dataRPD.id, id.var=c("subject","pred","dist","cond","position","region.id"), 
           measure.var="RPD.w")

M.id.w <- cast(temp, pred+dist+cond+position+region.id  ~ ., 
               function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

M.id.w.pred <- cast(temp, pred+position  ~ ., 
                    function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

M.id.w.cp<-subset(M.id.w,pred=="pred")
M.id.w.cp$pred<-droplevels(M.id.w.cp$pred)

M.id.w.sp<-subset(M.id.w,pred=="nopred")
M.id.w.sp$pred<-droplevels(M.id.w.sp$pred)

k<-1

M.id.w.orig<-M.id.w

colnames(M.id.w)[1]<-"Predictability"
M.id.w$Predictability<-factor(ifelse(M.id.w$Predictability=="pred","strong","weak"))

plotrpd<-ggplot(subset(M.id.w,position=="crit"), 
             aes(x=dist, y=M,group=Predictability)) + 
   geom_point(shape=21,fill="white",size=k*3) +
   geom_line(aes(linetype=Predictability),size=k) +
   geom_errorbar(aes(ymin=M-2*SE, ymax=M+2*SE),
                 width=.1,size=k)+
   xlab("Distance")+
   ylab("RPD (ms)")+
   labs(title="RPD at the Critical region [Verb]") +
   theme_bw() +
   labs(legend.position=c(.87, .6))

plotrpd<-plotrpd+theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 16, face = "bold"))+
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"))

M.id.w <- M.id.w.orig

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(plotfprt,plotrpd,cols=1)

## ----exptET2analysis,echo=FALSE------------------------------------------
data2<-read.table("ET/ET2-final.txt", header=T)
#head(data2)
## for some weird reason this subject column is 
## labeled subj:
colnames(data2)[1]<-("subject")
#length(unique(data2$subject))
#length(unique(data2$id))

#contrast coding :
## short -1, long 1
data2$dist<-ifelse(data2$cond%in%c("a","c"),-1,1)
## pred 1, -1
data2$pred<-ifelse(data2$cond%in%c("a","b"),1,-1)

#nested
data2$pred.dist <- ifelse(data2$cond=="a",-1,
                          ifelse(data2$cond=="b",1,0))
data2$nopred.dist <- ifelse(data2$cond=="c",-1,
                            ifelse(data2$cond=="d",1,0))


#### creating the column "position" to define the regions according to position:

data2$position<-NA

# crit :
data2$position[data2$cond=="a" & data2$roi==4]<- "crit"
data2$position[data2$cond=="b" & data2$roi==4]<- "crit"
data2$position[data2$cond=="c" & data2$roi==4]<- "crit"
data2$position[data2$cond=="d" & data2$roi==4]<- "crit"
#data2$position


#preverb or object :
data2$position[data2$cond=="a" & data2$roi==2] <- "dep"
data2$position[data2$cond=="b" & data2$roi==2]<- "dep"
data2$position[data2$cond=="c" & data2$roi==2]<- "dep"
data2$position[data2$cond=="d" & data2$roi==2]<- "dep"

# subject :
data2$position[data2$cond=="a" & data2$roi==1] <- "predep"
data2$position[data2$cond=="b" & data2$roi==1]<- "predep"
data2$position[data2$cond=="c" & data2$roi==1]<- "predep"
data2$position[data2$cond=="d" & data2$roi==1]<- "predep"

# intervener :
data2$position[data2$cond=="a" & data2$roi==3] <- "intervener"
data2$position[data2$cond=="b" & data2$roi==3]<- "intervener"
data2$position[data2$cond=="c" & data2$roi==3]<- "intervener"
data2$position[data2$cond=="d" & data2$roi==3]<- "intervener"

#post crit
data2$position[data2$cond=="a" & data2$roi==5] <- "postcrit"
data2$position[data2$cond=="b" & data2$roi==5]<- "postcrit"
data2$position[data2$cond=="c" & data2$roi==5]<- "postcrit"
data2$position[data2$cond=="d" & data2$roi==5]<- "postcrit"

#post :
data2$position[data2$cond=="a" & data2$roi==6] <- "post"
data2$position[data2$cond=="b" & data2$roi==6]<- "post"
data2$position[data2$cond=="c" & data2$roi==6]<- "post"
data2$position[data2$cond=="d" & data2$roi==6]<- "post"

#data2$position

etdata2<-data2

data2 <- na.omit(data2)

#head(data2)

a.regions<-c("predep","dep","intervener","crit","postcrit", "post")
b.regions<-c("predep","dep","intervener","crit","postcrit", "post")
c.regions<-c("predep","dep","intervener","crit","postcrit", "post")
d.regions<-c("predep","dep","intervener","crit","postcrit", "post")

regions<-c(a.regions,b.regions,c.regions,d.regions)
region.id<-rep(1:6,4)
cond.id<-rep(letters[1:4],each=6)

region.df<-data.frame(cond=factor(cond.id),
                      region.id=region.id,
                      roi=factor(regions))

## Expt 4 data analysis

#contrast coding reminder:
## short -1, long 1
data2$dist<-ifelse(data2$cond%in%c("a","c"),-1,1)
## pred 1, -1
data2$pred<-ifelse(data2$cond%in%c("a","b"),1,-1)

#nested
data2$pred.dist <- ifelse(data2$cond=="a",-1,
                          ifelse(data2$cond=="b",1,0))
data2$nopred.dist <- ifelse(data2$cond=="c",-1,
                            ifelse(data2$cond=="d",1,0))

#ET2.FPRT<-lmer(log(FPRT)~dist*pred+(1+dist*pred||subject)+(1+dist*pred||id),subset(data2,position=="crit" & FPRT!=0))
#summary(ET2.FPRT)
## keep max
#qqPlot(residuals(ET2.FPRT))

#ET2.RPD<-lmer(log(RPD)~dist*pred+(1+dist*pred||subject)+(1+dist*pred||id),subset(data2,position=="crit" & RPD!=0))
#summary(ET2.RPD)
#summary(rePCA(ET2.RPD))
#ET2.RPDfin<-lmer(log(RPD)~dist*pred+(1|subject)+(1+dist*pred||id),subset(data2,position=="crit" & RPD!=0))

#summary(ET2.RPDfin)
#qqPlot(residuals(ET2.RPDfin))

#ET2.FPRTnest<-lmer(log(FPRT)~pred+pred.dist+nopred.dist+(1+pred+pred.dist+nopred.dist||subject)+(1+pred+pred.dist+nopred.dist||id),subset(data2,position=="crit" & FPRT!=0))
#summary(ET2.FPRTnest)
#qqPlot(residuals(ET2.FPRTnest))

#ET2.RPDnest<-lmer(log(RPD)~pred+pred.dist+nopred.dist+(1+pred+pred.dist+nopred.dist||subject)+(1+pred+pred.dist+nopred.dist||id),subset(data2,position=="crit" & RPD!=0))
#summary(ET2.RPDnest)

#ET2.RPDnestfin<-lmer(log(RPD)~pred+pred.dist+nopred.dist+(1|subject)+(1+pred+nopred.dist||id),subset(data2,position=="crit" & RPD!=0))
#summary(ET2.RPDnestfin)
#qqPlot(residuals(ET2.RPDnestfin))

if(0){
stanm4fprt<-stan_lmer(log(FPRT)~dist*pred+
                        (1+dist*pred|subject)+
               (1+dist*pred|id),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=subset(data2,position=="crit" & FPRT!=0))

save(stanm4fprt,file="stanm4fprt.Rda")

stanm4nestedfprt<-stan_lmer(log(FPRT)~pred+pred.dist+nopred.dist+(1+pred+pred.dist+nopred.dist|subject)+
               (1+pred+pred.dist+nopred.dist|id),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=3000,
                  cores=4,
                  data=subset(data2,position=="crit" & FPRT!=0))

save(stanm4nestedfprt,file="stanm4nestedfprt.Rda")

## RPD:
stanm4rpd<-stan_lmer(log(RPD)~dist*pred+
                        (1+dist*pred|subject)+
               (1+dist*pred|id),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=3000,
                  cores=4,
                  data=subset(data2,position=="crit" & RPD!=0))

save(stanm4rpd,file="stanm4rpd.Rda")

stanm4nestedrpd<-stan_lmer(log(RPD)~pred+pred.dist+nopred.dist+(1+pred+pred.dist+nopred.dist|subject)+
               (1+pred+pred.dist+nopred.dist|id),
                  prior_intercept=normal(0,6),
                  prior=normal(0,1),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=4000,
                  cores=4,
                  data=subset(data2,position=="crit" & RPD!=0))

save(stanm4nestedrpd,file="stanm4nestedrpd.Rda")

}

load("stanm4fprt.Rda")
load("stanm4nestedfprt.Rda")

load("stanm4rpd.Rda")
load("stanm4nestedrpd.Rda")

stanm4fprt_tab<-summary_stan_model(stanm4fprt)
stanm4nestedfprt_tab<-summary_stan_model(stanm4nestedfprt)

stanm4rpd_tab<-summary_stan_model(stanm4rpd)
stanm4nestedrpd_tab<-summary_stan_model(stanm4nestedrpd)

## ----exp4questionresponseanalysis,echo=FALSE-----------------------------
etdata2q<-read.table("ET/etdata2qrespacc.txt",header=TRUE)
#head(etdata2q)

meanset2q<-round(with(etdata2q,tapply(resp,cond,mean))*100)

if(0){
stanglmeretqm2<-stan_glmer(resp~dist*pred+(1+dist+pred|subject)+
               (1+dist+pred|id),
               family=binomial(),
                     prior_intercept=student_t(df=2),
                     prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=3000,
                  cores=4,
                  data=etdata2q)

stanglmeretqm2nested<-stan_glmer(resp~pred+pred.dist+nopred.dist
                                 +(1+pred+pred.dist+nopred.dist|subject)+
               (1+pred+pred.dist+nopred.dist|id),
               family=binomial(),
                     prior_intercept=student_t(df=2),
                     prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=2000,
                  cores=4,
                  data=etdata2q)

save(stanglmeretqm2,file="stanglmeretqm2.Rda")

save(stanglmeretqm2nested,file="stanglmeretqm2nested.Rda")
}


load("stanglmeretqm2.Rda")
stanglmeretqm2_tab<-summary_stan_model(stanglmeretqm2)

load("stanglmeretqm2nested.Rda")
stanglmeretqm2nested_tab<-summary_stan_model(stanglmeretqm2nested)

## ----plotexp4,echo=FALSE-------------------------------------------------
## Plots
pred<-factor(ifelse(data2$cond%in%c("a","b"),"pred",
                    "nopred"),
             levels=c("pred","nopred"))
dist<-factor(ifelse(data2$cond%in%c("a","c"),"short",
                    "long"),
             levels=c("short","long"))

data2$pred<-pred
data2$dist<-dist

data2.uniq.reg<-ddply(data2, 
                      .(subject, id, cond, position),
                      summarize,
                      FPRT = FPRT,
                      RPD = RPD)

dataFPRT.rs <- melt(data2, 
                id=c("pred","dist","cond","position","subject"), 
                measure=c("FPRT"),
                na.rm=TRUE)

dataRPD.rs <- melt(data2, 
                id=c("pred","dist","cond","position","subject"), 
                measure=c("RPD"),
                na.rm=TRUE)

#get mean and no. of data points for each region, for each subject/condition
dataFPRT.id  <- data.frame(cast(dataFPRT.rs, 
                            subject+pred+dist+cond+position ~ ., 
                            function(x) c(FPRT=mean(x), N=length(x) ) ))

dataRPD.id  <- data.frame(cast(dataRPD.rs, 
                            subject+pred+dist+cond+position ~ ., 
                            function(x) c(RPD=mean(x), N=length(x) ) ))

## First-pass reading time
GM <- mean(tapply(dataFPRT.id$FPRT, dataFPRT.id$subject, mean))

#deviation from the grandmean after considering intra-subject variability
#removing between subject variance
dataFPRT.id <- ddply(dataFPRT.id, .(subject), 
                 transform, FPRT.w = FPRT - mean(FPRT) + GM)

temp<-melt(dataFPRT.id, id.var=c("subject","pred","dist","cond","position"), 
           measure.var="FPRT.w")

M.id.w <- cast(temp, pred+dist+cond+position  ~ ., 
               function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

M.id.w.pred <- cast(temp, pred+position  ~ ., 
                    function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

k<-1

colnames(M.id.w)[1]<-"Predictability"
M.id.w$Predictability<-factor(ifelse(M.id.w$Predictability=="pred","strong","weak"))


pFPRT<-ggplot(subset(M.id.w,position=="crit"), 
             aes(x=dist, y=M,group=Predictability)) + 
   geom_point(shape=21,fill="white",size=k*3) +
   geom_line(aes(linetype=Predictability),size=k) +
   geom_errorbar(aes(ymin=M-2*SE, ymax=M+2*SE),
                 width=.1,size=k)+
   xlab("Distance")+
   ylab("FPRT (ms)")+
   labs(title="FPRT at the Critical region [Verb]") +
   theme_bw() +
   labs(legend.position=c(.87, .6))

pFPRT<-pFPRT+theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 16, face = "bold"))+
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"))

M.id.w <- M.id.w.orig

## Regression-path duration
GM <- mean(tapply(dataRPD.id$RPD, dataRPD.id$subject, mean))

#deviation from the grandmean after considering intra-subject variability
#removing between subject variance
dataRPD.id <- ddply(dataRPD.id, .(subject), 
                 transform, RPD.w = RPD - mean(RPD) + GM)

temp<-melt(dataRPD.id, id.var=c("subject","pred","dist","cond","position"), 
           measure.var="RPD.w")

M.id.w <- cast(temp, pred+dist+cond+position  ~ ., 
               function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

M.id.w.pred <- cast(temp, pred+position  ~ ., 
                    function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x) ) )

k<-1

colnames(M.id.w)[1]<-"Predictability"
M.id.w$Predictability<-factor(ifelse(M.id.w$Predictability=="pred","strong","weak"))

pRPD<-ggplot(subset(M.id.w,position=="crit"), 
             aes(x=dist, y=M,group=Predictability)) + 
   geom_point(shape=21,fill="white",size=k*3) +
   geom_line(aes(linetype=Predictability),size=k) +
   geom_errorbar(aes(ymin=M-2*SE, ymax=M+2*SE),
                 width=.1,size=k)+
   xlab("Distance")+
   ylab("RPD (ms)")+
   labs(title="RPD at the Critical region [Verb]") +
   theme_bw() +
   labs(legend.position=c(.87, .6))

pRPD<-pRPD+theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 16, face = "bold"))+
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"))
M.id.w <- M.id.w.orig

multiplot(pFPRT,pRPD,cols=1)

## ----summarizeresults,echo=FALSE-----------------------------------------
## prepare summary
stanm1_tab<-stanm1_tab[2:4,]
stanm2_tab<-stanm2_tab[2:4,]
stanm3fprt_tab<-stanm3fprt_tab[2:4,]
stanm3rpd_tab<-stanm3rpd_tab[2:4,]
stanm4fprt_tab<-stanm4fprt_tab[2:4,]
stanm4rpd_tab<-stanm4rpd_tab[2:4,]

stanm1_tab$experiment<-"e1"
stanm2_tab$experiment<-"e2"
stanm3fprt_tab$experiment<-"e3"
stanm3rpd_tab$experiment<-"e3"
stanm4fprt_tab$experiment<-"e4"
stanm4rpd_tab$experiment<-"e4"
stanm1_tab<-as.data.frame(stanm1_tab[c(6,1:4)])
stanm2_tab<-as.data.frame(stanm2_tab[c(6,1:4)])
stanm3fprt_tab<-as.data.frame(stanm3fprt_tab[c(6,1:4)])
stanm3rpd_tab<-as.data.frame(stanm3rpd_tab[c(6,1:4)])
stanm4fprt_tab<-as.data.frame(stanm4fprt_tab[c(6,1:4)])
stanm4rpd_tab<-as.data.frame(stanm4rpd_tab[c(6,1:4)])

sprdata<-rbind(stanm1_tab,stanm2_tab)
etdatafprt<-rbind(stanm3fprt_tab,stanm4fprt_tab)
etdatarpd<-rbind(stanm3rpd_tab,stanm4rpd_tab)

colnames(sprdata)[3]<-"estimate"
colnames(etdatafprt)[3]<-"estimate"
colnames(etdatarpd)[3]<-"estimate"

sprdata$comparison<-factor(sprdata$comparison,
                           levels=c("dist","pred","dist:pred"))

etdatafprt$comparison<-factor(etdatafprt$comparison,
                           levels=c("dist","pred","dist:pred"))

etdatarpd$comparison<-factor(etdatarpd$comparison,
                           levels=c("dist","pred","dist:pred"))

#factor(sprdata$comparison)

sprdata$estimate<-as.numeric(as.character(sprdata$estimate))
sprdata$lower<-as.numeric(as.character(sprdata$lower))
sprdata$upper<-as.numeric(as.character(sprdata$upper))

etdatafprt$estimate<-as.numeric(as.character(etdatafprt$estimate))
etdatafprt$lower<-as.numeric(as.character(etdatafprt$lower))
etdatafprt$upper<-as.numeric(as.character(etdatafprt$upper))

etdatarpd$estimate<-as.numeric(as.character(etdatarpd$estimate))
etdatarpd$lower<-as.numeric(as.character(etdatarpd$lower))
etdatarpd$upper<-as.numeric(as.character(etdatarpd$upper))

#str(sprdata)

pd<-position_dodge(0.3)

decorate<-theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+theme(legend.text = element_text(colour="black", size = 16, face = "bold"))+
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"))

sprplot<-ggplot(sprdata, aes(x=comparison, 
                             y=estimate, colour=experiment, group=experiment)) +
        geom_errorbar(aes(ymin=lower, ymax=upper),
                      width=.2, size=0.25, colour="black", position=pd) +
       labs(title="Log RT") +
  geom_hline(yintercept=0)+
geom_point(position=pd, size=2.5)+theme_bw()

sprplot<-sprplot+decorate+ylim(-.1,.1)

## et data:
etplotfprt<-ggplot(etdatafprt, aes(x=comparison, 
                             y=estimate, colour=experiment, group=experiment)) +
        geom_errorbar(aes(ymin=lower, ymax=upper),
                      width=.2, size=0.25, colour="black", position=pd) +
       labs(title="Log FPRT") +
  geom_hline(yintercept=0)+
geom_point(position=pd, size=2.5)+theme_bw()

etplotfprt<-etplotfprt+decorate

etplotrpd<-ggplot(etdatarpd, aes(x=comparison, 
                             y=estimate, colour=experiment, group=experiment)) +
        geom_errorbar(aes(ymin=lower, ymax=upper),
                      width=.2, size=0.25, colour="black", position=pd) +
       labs(title="Log RPD") +
  geom_hline(yintercept=0)+
geom_point(position=pd, size=2.5)+theme_bw()

etplotrpd<-etplotrpd+decorate

multiplot(sprplot,etplotfprt,etplotrpd,cols=1)

## ----entropycode,echo=FALSE----------------------------------------------
d<-read.table('Pretests/entropy/sentcomp.txt', header=T)


#subset experiment 1 data
exp1<-subset(d, Pre.test==1)
exp1$subj.ID<-factor(exp1$subj.ID)

#xtabs(~subj.ID+cond,exp1)
## H, N, M are duplicated, mistake in assigning unique ids
## fixed below:
exp1$subj.ID<-paste(exp1$LatSq.list,exp1$subj.ID,sep="")
#xtabs(~subj.ID+cond,exp1)

library(plyr)
temp<-ddply(exp1,.(item.ID,cond,verb),nrow)
#head(temp)

#in three instances total count !=8
#1b     7
#3a     7
#14b    7
temp1<-ddply(temp,.(item.ID,cond),summarize,count=sum(V1))
#head(temp1)

exp1.verbcount<-merge(temp, temp1, by.x=c("item.ID", "cond"))

exp1.verbcount$prob <- exp1.verbcount$V1/exp1.verbcount$count
#head(exp1.verbcount)

exp1.verbcount[] <- lapply(exp1.verbcount, as.character)

## code rechecked by SV 21 Feb 2016 (correct)
for(i in 1:length(unique(exp1.verbcount$item.ID))) #iterate over each item
{
  cond <- unique(exp1.verbcount$cond)
  
  for(j in 1:length(cond)) #iterate over each cond
  {
    #get number of unique verbs of an item and condition
    verbs <- length(exp1.verbcount[exp1.verbcount$item.ID==i&exp1.verbcount$cond==cond[j],]$prob)
    
    sum = 0
    #compute entropy
    for(k in 1:verbs)
    {
      sum = sum + (as.numeric(exp1.verbcount[exp1.verbcount$item.ID==i&exp1.verbcount$cond==cond[j],]$prob[k])
                   *log2(as.numeric(exp1.verbcount[exp1.verbcount$item.ID==i&exp1.verbcount$cond==cond[j],]$prob[k])))
    }
    
    exp1.verbcount$entropy[exp1.verbcount$item.ID==i&exp1.verbcount$cond==cond[j]] <- -1 * sum
  }
}

#head(exp1.verbcount)

exp1.frame<-ddply(exp1.verbcount,.(item.ID,cond,count,entropy), nrow)
colnames(exp1.frame)[5] <- "unique.verbs"
#head(exp1.frame)

exp1.frame$cond<-factor(exp1.frame$cond)

#entropy
means<-with(exp1.frame,tapply(entropy,cond,mean))
stddev<-with(exp1.frame,tapply(entropy,cond,sd))
stderr<-stddev/sqrt(36)

## entropy plot for paper:
Distance<-factor(c(rep(c("short","long"),2)),levels=c("short","long"))
Predicate_Type<-factor(c(rep(c("complex","simple"),each=2)),levels=c("complex","simple"))

entropydate1<-data.frame(Predicate_Type=Predicate_Type,Distance=Distance,ent=means,se=stderr)

#library(ggplot2)

entp1<-ggplot(entropydate1, aes(x=Predicate_Type, y=ent, fill=Distance)) +
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=ent-2*se, ymax=ent+2*se), 
                position=position_dodge(0.9),width=.2)+
  xlab("Predicate Type")+ylab("Entropy")+ggtitle("Intervener RC+PP (Expts 1,3)")+theme_bw()

## difference 1 lower than original calculation
#entp1

#====================
#experiment 2

exp2<-subset(d, Pre.test==2)
exp2$subj.ID<-factor(exp2$subj.ID)
#xtabs(~subj.ID+cond,exp2)

temp<-ddply(exp2,.(item.ID,cond,verb),nrow)
#head(temp)

#one instance of total count !=8
#15c     6
temp1<-ddply(temp,.(item.ID,cond),summarize,count=sum(V1))
#head(temp1)

exp2.verbcount<-merge(temp, temp1, by.x=c("item.ID", "cond"))

exp2.verbcount$prob <- exp2.verbcount$V1/exp2.verbcount$count
#head(exp2.verbcount)

exp2.verbcount[] <- lapply(exp2.verbcount, as.character)

for(i in 1:length(unique(exp2.verbcount$item.ID))) #iterate over each item
{
  cond <- unique(exp2.verbcount$cond)
  
  for(j in 1:length(cond)) #iterate over each cond
  {
    #get number of unique verbs of an item and condition
    verbs <- length(exp2.verbcount[exp2.verbcount$item.ID==i&exp2.verbcount$cond==cond[j],]$prob)
    
    sum = 0
    #compute entropy
    for(k in 1:verbs)
    {
      sum = sum + (as.numeric(exp2.verbcount[exp2.verbcount$item.ID==i&exp2.verbcount$cond==cond[j],]$prob[k])
                   *log2(as.numeric(exp2.verbcount[exp2.verbcount$item.ID==i&exp2.verbcount$cond==cond[j],]$prob[k])))
    }
    
    exp2.verbcount$entropy[exp2.verbcount$item.ID==i&exp2.verbcount$cond==cond[j]] <- -1 * sum
  }
}

exp2.frame<-ddply(exp2.verbcount,.(item.ID,cond,count,entropy), nrow)
colnames(exp2.frame)[5] <- "unique.verbs"
#head(exp2.frame)

exp2.frame$cond<-factor(exp2.frame$cond)

#entropy
means<-with(exp2.frame,tapply(entropy,cond,mean))
stddev<-with(exp2.frame,tapply(entropy,cond,sd))
stderr<-stddev/sqrt(36)

Distance<-factor(c(rep(c("short","long"),2)),levels=c("short",
                                                      "long"))
Predicate_Type<-factor(c(rep(c("complex","simple"),each=2)),levels=c("complex","simple"))

entropydate2<-data.frame(Predicate_Type=Predicate_Type,Distance=Distance,ent=means,se=stderr)

entp2<-ggplot(entropydate2, aes(x=Predicate_Type, y=ent, fill=Distance)) +
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=ent-2*se, ymax=ent+2*se), 
                position=position_dodge(0.9),width=.2)+
  xlab("Predicate Type")+ylab("Entropy")+ggtitle("Intervener PP (Expts 2, 4)")+scale_y_continuous(limits = c(0, 2))+theme_bw()

#entp2

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## ----entropyplot,echo=FALSE----------------------------------------------
multiplot(entp1,entp2,cols=2)

## ----entropydiffse1e2,echo=FALSE-----------------------------------------
#high predictibility conditions long vs short difference in entropy (Exp1 vs Exp2)
diffe1ba<-subset(exp1.frame,cond=="b")$entropy-subset(exp1.frame,cond=="a")$entropy
e1entdata<-data.frame(item.ID=subset(exp1.frame,cond=="b")$item.ID,diffent=diffe1ba,exp=factor("e1"))

diffe2ba<-subset(exp2.frame,cond=="b")$entropy-subset(exp2.frame,cond=="a")$entropy
e2entdata<-data.frame(item.ID=subset(exp2.frame,cond=="b")$item.ID,diffent=diffe2ba,exp=factor("e2"))

#t.test(diffe1ba,diffe2ba,paired=T)

e1e2entdata<-rbind(e1entdata,e2entdata)

e1e2entdata$experiment<-ifelse(e1e2entdata$exp=="e1",1,-1)
#summary(lmer(diffent~experiment+(1|item.ID),e1e2entdata))

## stan model
if(0){
diffente1e2stan<-stan_lmer(diffent~experiment+
                    (1+experiment|item.ID),
                  prior_intercept=student_t(df=2),
                  prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  e1e2entdata,
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=2000,
                  cores=4)

save(diffente1e2stan,file="diffente1e2stan.Rda")
}
load("diffente1e2stan.Rda")
diffente1e2stan_tab<-summary_stan_model(diffente1e2stan,nfixefs=1)
#str(diffente1e2stan_tab)

## ----entropyrtanalysis,echo=FALSE----------------------------------------
exp1entropy<-exp1.frame[,c(1,2,4)]
exp2entropy<-exp2.frame[,c(1,2,4)]

#SPR exp1 data
data1<-read.table("data_spr/Persiane1.txt", header=T)
data1$pos<-factor(data1$pos)
data1$resp<-factor(data1$resp)
data1$cond<-factor(data1$cond)
## short -1, long 1
data1$dist<-ifelse(data1$cond%in%c("a","c"),-1,1)
## pred 1, -1
data1$pred<-ifelse(data1$cond%in%c("a","b"),1,-1)

#nested
data1$pred.dist <- ifelse(data1$cond=="a",-1,
                          ifelse(data1$cond=="b",1,0))
data1$nopred.dist <- ifelse(data1$cond=="c",-1,
                            ifelse(data1$cond=="d",1,0))

## merge with data:
data1crit<-subset(data1,roi=="crit" & rt<3000)

data1critent<-merge(data1crit,exp1entropy,by.x=c("item","cond"),
                    by.y=c("item.ID","cond"))

## sanity check
#dim(data1crit)
#dim(data1critent)
#summary(data1critent)

## center:
data1critent$c_ent<-scale(data1critent$entropy,scale=F)

library(lme4)

m1ent<-lmer(log(rt)~pred*dist*c_ent+(1+dist+pred:dist||subj)+(1+c_ent||item),data1critent)
#summary(m1ent)

## stan:
if(0){
rtente1stan<-stan_lmer(log(rt)~pred*dist*c_ent+
                             +(1+pred*dist*c_ent|subj)+
                    (1+pred*dist*c_ent|item),
                  prior_intercept=student_t(df=2),
                  prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                data1critent,
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=2000,
                  cores=4)

save(rtente1stan,file="rtente1stan.Rda")
}

load("rtente1stan.Rda")
rtente1stan_tab<-summary_stan_model(rtente1stan,nfixefs=7)

## ----grammaticalitysentcomp,echo=FALSE-----------------------------------
#sentence completion target/condition & grammaticality/condition analysis
exp1abo<-read.table("Pretests/entropy/exp1aborevised.txt",header=TRUE)
#xtabs(~subject+cond,exp1abo)

#grammaticality (experiment 1 a vs b)

#grammaticality in the long vs short condition in experiment 1
means<-with(exp1abo,tapply(gram,cond,mean))

## just checking:
#xtabs(~subject+condition,exp1abo)
#summary(exp1abo)
#xtabs(~subject+condition,exp1abo)
#xtabs(~subject+item.ID,exp1abo)
#xtabs(~item.ID+condition,exp1abo)
#xtabs(~item.ID+cond,exp1)
#sort(subset(exp1abo,subject=="A1")$item.ID)
#sort(subset(exp1abo,subject=="A1")$item.ID)

## doesn't converge:
#mglmergram<-glmer(gram~condition+
#                        (1|subject)+
#                        (1|item.ID),
#                  family=binomial(),
#                  exp1abo)

if(0){
mstangram<-stan_glmer(gram~condition+
                        (1+condition|subject)+
                        (1+condition|item.ID),
                      family=binomial(),
                      prior_intercept=student_t(df=2),
                      prior=student_t(df=2),
                      prior_covariance=decov(regularization=2),
                      exp1abo,
                      algorithm="sampling",
                      adapt_delta=0.9999,
                      chains=4,
                      iter=2000,
                      cores=4)

save(mstangram,file="mstangram.Rda")
}

load("mstangram.Rda")

#print(mstangram)

mstangram_tab<-summary_stan_model(mstangram,
                                  nfixefs=1)

logodds<-as.numeric(as.character(mstangram_tab[2,2]))
gramodds<-exp(logodds)
logoddslower<-as.numeric(as.character(mstangram_tab[2,3]))
gramoddslower<-exp(logoddslower)
logoddsupper<-as.numeric(as.character(mstangram_tab[2,4]))
gramoddsupper<-exp(logoddsupper)
probneg<-round(as.numeric(as.character(mstangram_tab[2,5])),digits=2)

## ----probexpectationweakeninge1,echo=FALSE-------------------------------
## load sentence completion data:
d<-read.table('Pretests/entropy/sentcomp.txt', header=T)

## Expt 1 analysis
exp1<-subset(d, Pre.test==1)
## Isolate a,b conditions:
exp1ab<-subset(exp1,cond%in%c("a","b"))
exp1ab$cond<-factor(exp1ab$cond)
#summary(exp1ab)

target_verbs_ab<-c("kardan","zadan",
                "kardan","kardan",
                "kardan","kardan",
                "kardan","dashtan",
                "bordan","kardan",
                "zadan","zadan",
                "zadan","zadan",
                "zadan","zadan",
                "kardan","kardan",
                "kardan","kardan",
                "dadan","kardan",
                "dadan","dadan",
                "goftan","kardan",
                "kardan","kardan",
                "gozashtan","zadan",
                "kardan","kardan",
                "kardan","kardan",
                "gereftan","kardan")

targets<-data.frame(item=rep(1:36,each=2),
                    cond=rep(letters[1:2],36),
                    target_verbs=rep(target_verbs_ab,each=2))

nrows<-dim(exp1ab)[1]

## Next, create a column marking "correct completion"
hit<-rep(NA,nrows)

## identify exact matches with target:
for(i in 1:nrows){
  ## get i-th row:
  tmp<-exp1ab[i,]
  itemid<-tmp$item.ID
  condition<-as.character(tmp$cond)
  tmpverb<-as.character(tmp$verb)
  ## find appropriate row in targets data frame:
  target_row<-subset(targets,item==itemid & 
                       cond==condition)
  targetverb<-as.character(target_row$target_verbs)
  if(targetverb==tmpverb){
    hit[i]<-1} else {hit[i]<-0}
}

## 1 if target verb produced, 0 otherwise
exp1ab$target<-hit

exp1ab$condition<-ifelse(exp1ab$cond=="b",1,-1)
exp1ab$subj.ID<-factor(exp1ab$subj.ID)

## Fix problem by pasting list no. to make subjects unique:
exp1ab$subject<-factor(paste(exp1ab$subj.ID,exp1ab$LatSq.list,sep=""))

if(0){
#target completion (experiment 1, a v b)
mstanhits<-stan_glmer(target~condition+(1+condition|subject)+(condition|item.ID),
                  family=binomial(),
                  prior_intercept=student_t(df=2),
                  prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  exp1ab,
                  algorithm="sampling",
                  adapt_delta=0.9999,
                  chains=4,
                  iter=2000,
                  cores=4)

save(mstanhits,file="mstanhits.Rda")
}

load("mstanhits.Rda")
mstanhits_tab<-summary_stan_model(mstanhits,nfixefs=1)

## ----probexpectationweakeninge2,echo=FALSE-------------------------------
## no reduction in grammaticality of target in experiment 2:
exp2abo<-read.table("Pretests/entropy/expt2ab.txt",header=TRUE)
#with(exp2abo,tapply(gram,cond,mean))

if(0){
  mstan.target.expt2<-stan_glmer(target~condition+(1+condition|subj.ID)+(condition|item.ID),
                  family=binomial(),
                  prior_intercept=student_t(df=2),
                  prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  exp2abo,
                  algorithm="sampling",
                  adapt_delta=0.9999,                  
                  chains=4,
                  iter=2000,
                  cores=4)

save(mstan.target.expt2,file="mstan.target.expt2.Rda")  
}
load("mstan.target.expt2.Rda")
mstan.target.expt2_tab<-summary_stan_model(mstan.target.expt2,nfixefs=1)  
mne2<-round(as.numeric(as.character(mstan.target.expt2_tab[2,2])),digits=2)
lowere2<-round(as.numeric(as.character(mstan.target.expt2_tab[2,3])),digits=2)
uppere2<-round(as.numeric(as.character(mstan.target.expt2_tab[2,4])),digits=2)
probe2<-round(as.numeric(as.character(mstan.target.expt2_tab[2,5])),digits=2)
oddsratioe2<-exp(mne2)

