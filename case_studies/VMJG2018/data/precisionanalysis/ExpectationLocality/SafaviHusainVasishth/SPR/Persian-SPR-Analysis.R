#setwd("C:/Users/Farnoosh/Desktop")

## remove: 5,9, 26, 32

data1<-read.table("Persiane1-regions.DAT", header=T)
data2<-read.table("Persiane2-regions.DAT", header=T)

#colnames(data1)<-c("subj", "item","cond","pos","resp","rt","roi")
#colnames(data1)<-c("subj","expt","item","cond","pos","word","resp","rt","roi")

#####################    SPR 1
head(data1)

data1$pos<-factor(data1$pos)
data1$resp<-factor(data1$resp)
data1$cond<-factor(data1$cond)
#################
#questions (response accuracy)
data1.q <- subset(data1, pos=="?")
data1.q$resp<- as.numeric(as.character(data1.q$resp))
meansq<-round(100*with(data1.q,tapply(resp,cond,mean)),digits=0)
meansq
barplot(meansq,beside=T)

#################
#data1$cond <- droplevels(data1$cond)
means.reg<-with(data1,tapply(rt,IND=list(cond,roi),mean))
barplot(means.reg,beside=T)

means<-with(subset(data1, roi=="crit"),tapply(rt,cond,mean))
barplot(means,beside=T)

## short -1, long 1
data1$dist<-ifelse(data1$cond%in%c("a","c"),-1,1)
## pred 1, -1
data1$pred<-ifelse(data1$cond%in%c("a","b"),-1,1)

#nested
data1$pred.dist <- ifelse(data1$cond=="a",-1,
                          ifelse(data1$cond=="b",1,0))
data1$nopred.dist <- ifelse(data1$cond=="c",-1,
                         ifelse(data1$cond=="d",1,0))

library(lme4)
#main effect of distance: rt(b,d) > rt(a,c), this is driven by the c,d conditions
#main effect of prediction (conditions a,b read faster than c,d)
#marginal interaction (t=-1.53)

library(lattice)
bwplot(log(rt)~cond,subset(data1,roi=="crit"))

## 4 extreme data points removed: 0.02%
## rePCA used to eliminate RE components:
m1<-lmer(log(rt)~dist*pred+(1+dist+dist:pred||subj)+(1+pred||item),subset(data1,roi=="crit" & rt<3000))
summary(m1)

library(car)
qqPlot(residuals(m1))

library(RePsychLing)
summary(rePCA(m1))

#rt(d) > rt(c)
m1.nested<-lmer(log(rt)~pred+pred.dist+nopred.dist+(1+pred.dist+nopred.dist||subj)+(1+pred||item),subset(data1,roi=="crit" & rt<3000))
summary(rePCA(m1.nested))

summary(m1.nested)

##### responses : 

qdata1<-subset(data1, pos=="?")
qdata1$resp<-as.numeric(as.character(qdata1$resp))

round(100*with(qdata1,tapply(resp,cond,mean)),
      digits=0)

xtabs(~subj+cond,subset(data1, pos=="?"))

xtabs(~item+cond,subset(data1, pos=="?"))

m1.q<-glmer(resp~dist*pred+(1|subj)+(1|item),subset(data1, pos=="?"), family=binomial())


m1.nested.q<-glmer(resp~pred+pred.dist+nopred.dist+(1|subj)+(1|item),subset(data1, pos=="?"), family=binomial())
summary(m1.q)
summary(m1.nested.q)
##############################

### Data preparation for plotting:
library(plyr)

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

library(reshape)
library(ggplot2)

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

(p1a<-ggplot(subset(M.id.w,roi=="crit"), 
             aes(x=dist, y=M,group=Expectation)) + 
               geom_point(shape=21,fill="white",size=k*3) +
               geom_line(aes(linetype=Expectation),size=k) +
               geom_errorbar(aes(ymin=M-2*SE, ymax=M+2*SE),
                             width=.1,size=k)+
                               xlab("Distance")+
                               ylab("reading time [Raw RT in ms]")+
                               scale_colour_hue(name="Predicate type",
                                                breaks=c("strong-exp", "weak-exp"),
                                                labels=c("CP", "SP"),
                                                l=40)+                     
                                                  labs(title="Critical region [Verb]") +
                                                  theme_bw() +
                                                  labs(legend.position=c(.87, .6))
 )

#############




M.id.w.cp<-subset(M.id.w,Expectation=="strong-exp")
M.id.w.cp$Expectation<-droplevels(M.id.w.cp$Expectation)

M.id.w.sp<-subset(M.id.w,Expectation=="weak-exp")
M.id.w.sp$Expectation<-droplevels(M.id.w.sp$Expectation)

## function for generic by-region plots (effect of distance within CP vs. SP):
byregion.plot<-function(data,
                        mytitle,k=1,
                        x.lab="position",
                        y.lab="reading time [Raw rt]"){
  ggplot(data,aes(x=region.id,y=M,
                  group=dist)) + 
    geom_point(shape=21,size=k*2) +
    geom_line(aes(linetype=dist),size=k) +
    geom_errorbar(aes(ymin=M-2*SE, 
                      ymax=M+2*SE),
                  width=.1,size=k)+
    #xlab(x.lab)+
    scale_x_continuous(x.lab, breaks = 1:6, labels = c("predep","dep","precrit","crit","postcrit","post"))+
    #ylab(y.lab)+             
    #opts(title=mytitle) +
    #theme_bw()+
    #theme(axis.text.x = element_text(size=7))
    labs(x ='\nPosition', y = 'Reading time [msec]\n')+
    theme_bw()+
    ggtitle(mytitle)}

(plot.cp<-byregion.plot(M.id.w.cp,
                        mytitle="Effect of distance [strong expectation]",k=1,
                        x.lab="position",y.lab="reading time [Raw rt]")
)


(plot.sp<-byregion.plot(M.id.w.sp,
                        mytitle="Effect of distance [weak expectation]",k=1,
                        x.lab="position",y.lab="reading time [log ms]")
)


M.id.w.short<-subset(M.id.w,dist=="short")
M.id.w.short$dist <- droplevels(M.id.w.short$dist)

M.id.w.long<-subset(M.id.w,dist=="long")
M.id.w.long$dist <- droplevels(M.id.w.long$dist)

byregion.plot<-function(data,
                        mytitle,k=1,
                        x.lab="position",
                        y.lab="reading time [msec]"){
  ggplot(data,aes(x=region.id,y=M,
                  group=Expectation)) + 
    geom_point(shape=21,size=k*2) +
    geom_line(aes(linetype=Expectation),size=k) +
    geom_errorbar(aes(ymin=M-2*SE, 
                      ymax=M+2*SE),
                  width=.1,size=k)+
    #xlab(x.lab)+
    scale_x_continuous(x.lab, breaks = 1:6, labels = c("predep","dep","precrit","crit","postcrit","post"))+
    #ylab(y.lab)+             
    #opts(title=mytitle) +
    #theme_bw()+
    #theme(axis.text.x = element_text(size=7))
    labs(x ='\nPosition', y = 'Reading time [msec]\n')+
    theme_bw()+
    ggtitle(mytitle)}


(byregion.plot(M.id.w.long,
               mytitle="Effect of expectation (long conditions)",k=1,
               x.lab="position",y.lab="reading time [msec]")
)

(byregion.plot(M.id.w.short,
               mytitle="Effect of expectation (short conditions)",k=1,
               x.lab="position",y.lab="reading time [msec]")
)



################    SPR 2

head(data2)

data2$pos<-factor(data2$pos)
data2$resp<-factor(data2$resp)
data2$cond<-factor(data2$cond)
#################
#questions (response accuracy)
data2.q <- subset(data2, pos=="?")
data2.q$resp<- as.numeric(as.character(data2.q$resp))
meansq<-round(100*with(data2.q,tapply(resp,cond,mean)),digits=0)
meansq
barplot(meansq,beside=T)

#################
#data2$cond <- droplevels(data2$cond)
means.reg<-with(data2,tapply(rt,IND=list(cond,roi),mean))
barplot(means.reg,beside=T)

means<-with(subset(data2, roi=="crit"),tapply(rt,cond,mean))
barplot(means,beside=T)

## short -1, long 1
data2$dist<-ifelse(data2$cond%in%c("a","c"),-1,1)
## pred 1, -1
data2$pred<-ifelse(data2$cond%in%c("a","b"),-1,1)

#nested
data2$pred.dist <- ifelse(data2$cond=="a",-1,
                          ifelse(data2$cond=="b",1,0))
data2$nopred.dist <- ifelse(data2$cond=="c",-1,
                            ifelse(data2$cond=="d",1,0))

datae2<-data2

library(lme4)

#main effect of distance (long conditions b,d > short conditions a,c)
#main effect of prediction (CP conditions a,b < SP conditions c,d), main effect of dist: locality

library(lattice)
bwplot(log(rt)~cond,subset(data2,roi=="crit"))

## 4 extreme data points removed: 0.02%
m2<-lmer(log(rt)~dist*pred+(1+dist+dist:pred||subj)+(1+dist+dist:pred||item),subset(data2,roi=="crit" & rt<3000))

summary(rePCA(m2))

summary(m2)

#rt(d) > rt(c)
m2.nested<-lmer(log(rt)~pred+pred.dist+nopred.dist+(1+pred.dist+nopred.dist||subj)+(1+pred.dist+nopred.dist||item),subset(data2,roi=="crit" & rt<3000))

summary(rePCA(m2.nested))

summary(m2.nested)




##### responses : 

m2.q<-glmer(resp~dist*pred+(1|subj)+(1|item),subset(data2, pos=="?"), family=binomial())
m2.nested.q<-glmer(resp~pred+pred.dist+nopred.dist+(1|subj)+(1|item),subset(data2, pos=="?"), family=binomial())
summary(m2.q)
summary(m2.nested.q)
##############################

### Data preparation for plotting:
library(plyr)

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
library(reshape)
library(ggplot2)
 

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

(p1a<-ggplot(subset(M.id.w,roi=="crit"), 
             aes(x=dist, y=M,group=Expectation)) + 
   geom_point(shape=21,fill="white",size=k*3) +
   geom_line(aes(linetype=Expectation),size=k) +
   geom_errorbar(aes(ymin=M-2*SE, ymax=M+2*SE),
                 width=.1,size=k)+
   xlab("Distance")+
   ylab("reading time [Raw RT in ms]")+
   scale_colour_hue(name="Predicate type",
                    breaks=c("strong-exp", "weak-exp"),
                    labels=c("CP", "SP"),
                    l=40)+                     
   labs(title="Critical region [Verb]") +
   theme_bw() +
   labs(legend.position=c(.87, .6))
)

#############



M.id.w.cp<-subset(M.id.w,Expectation=="strong-exp")
M.id.w.cp$Expectation<-droplevels(M.id.w.cp$Expectation)

M.id.w.sp<-subset(M.id.w,Expectation=="weak-exp")
M.id.w.sp$Expectation<-droplevels(M.id.w.sp$Expectation)

## function for generic by-region plots (effect of distance within CP vs. SP):
byregion.plot<-function(data,
                        mytitle,k=1,
                        x.lab="position",
                        y.lab="reading time [Raw rt]"){
  ggplot(data,aes(x=region.id,y=M,
                  group=dist)) + 
    geom_point(shape=21,size=k*2) +
    geom_line(aes(linetype=dist),size=k) +
    geom_errorbar(aes(ymin=M-2*SE, 
                      ymax=M+2*SE),
                  width=.1,size=k)+
    #xlab(x.lab)+
    scale_x_continuous(x.lab, breaks = 1:6, labels = c("predep","dep","precrit","crit","postcrit","post"))+
    #ylab(y.lab)+             
    #opts(title=mytitle) +
    #theme_bw()+
    #theme(axis.text.x = element_text(size=7))
    labs(x ='\nPosition', y = 'Reading time [msec]\n')+
    theme_bw()+
    ggtitle(mytitle)}

(plot.cp<-byregion.plot(M.id.w.cp,
                        mytitle="Effect of distance [strong expectation]",k=1,
                        x.lab="position",y.lab="reading time [Raw rt]")
)


(plot.sp<-byregion.plot(M.id.w.sp,
                        mytitle="Effect of distance [weak expectation]",k=1,
                        x.lab="position",y.lab="reading time [log ms]")
)


M.id.w.short<-subset(M.id.w,dist=="short")
M.id.w.short$dist <- droplevels(M.id.w.short$dist)

M.id.w.long<-subset(M.id.w,dist=="long")
M.id.w.long$dist <- droplevels(M.id.w.long$dist)

byregion.plot<-function(data,
                        mytitle,k=1,
                        x.lab="position",
                        y.lab="reading time [msec]"){
  ggplot(data,aes(x=region.id,y=M,
                  group=Expectation)) + 
    geom_point(shape=21,size=k*2) +
    geom_line(aes(linetype=Expectation),size=k) +
    geom_errorbar(aes(ymin=M-2*SE, 
                      ymax=M+2*SE),
                  width=.1,size=k)+
    #xlab(x.lab)+
    scale_x_continuous(x.lab, breaks = 1:6, labels = c("predep","dep","precrit","crit","postcrit","post"))+
    #ylab(y.lab)+             
    #opts(title=mytitle) +
    #theme_bw()+
    #theme(axis.text.x = element_text(size=7))
    labs(x ='\nPosition', y = 'Reading time [msec]\n')+
    theme_bw()+
    ggtitle(mytitle)}


(byregion.plot(M.id.w.long,
               mytitle="Effect of expectation (long conditions)",k=1,
               x.lab="position",y.lab="reading time [msec]")
)

(byregion.plot(M.id.w.short,
               mytitle="Effect of expectation (short conditions)",k=1,
               x.lab="position",y.lab="reading time [msec]")
)



library(lme4)
#################### checking for spillover effect for SPR 1 :
m1_crit_full<-lmer(log(rt)~dist*pred+(dist*pred||subj)+
                     (dist*pred||item),
                   subset(data1,roi=="crit"))
summary(m1_crit_full)


## 6/1542 extreme data points removed (0.4%):
m1_crit<-lmer(log(rt)~dist*pred+(dist*pred||subj)+
                (dist*pred||item),
              subset(data1,roi=="crit" & rt<3000))
round(summary(m1_crit)$coef,2)

library(car)
qqPlot(residuals(m1_crit))

#main effect of distance
#main effect of prediction
m1_crit<-lmer(log(rt)~dist*pred+(1|subj)+(1|item),subset(data1,roi=="postcrit"))
summary(m1_crit)

qqPlot(residuals(m1_crit))

m1.nested<-lmer(log(rt)~pred+pred.dist+nopred.dist+(1|subj)+(1|item),subset(data1,roi=="crit"))
summary(m1.nested)

m1.nested<-lmer(log(rt)~pred+pred.dist+nopred.dist+
                  (pred+pred.dist+nopred.dist||subj)+
                  (pred+pred.dist+nopred.dist||item),
                subset(data1,roi=="postcrit" & rt<3000))

summary(m1.nested)

qqPlot(residuals(m1.nested))

##############################
#checking for any spillover from the precritical region
head(data1)
library(plyr)
##SV: the mistake is here:
#summing up rts across identical regions for each item
data1.uniq.reg<-ddply(data1, .(subj, item, cond, roi), summarize, rt = sum(rt))




crit.data <- subset(data1.uniq.reg, roi=='crit')

critdata<-(subset(data1,roi=="crit"))

xtabs(~item+cond,critdata)

subjlist<-unique(critdata$subj)
itemlist<-unique(critdata$item)
for(i in subjlist){
  for(j in itemlist){
    ## should always print out 1 
    print(dim(subset(critdata,item==j & subj==i))[1])
  }
}
## so there is only one region called crit in every row
## of the data frame for each subj and item. 
## The spillover region is the region preceding the critical one:

temp<-data1[!is.na(data2$roi),]
dim(temp)
unique(temp$roi=="crit")


n<-dim(temp)[1]
getprecritrt<-rep(NA,n)
## walk through data frame:
for(i in 1:n){
  if(temp[i,]$roi=="crit"){
    ## get rt before current pos:
    getprecritrt[i]<-temp[i-1,]$rt
  }
}

## get rid of NA's:
precritrt<-getprecritrt[!is.na(getprecritrt)]
## matches up:
length(precritrt)
str(precritrt)
dim(crit.data)

## center precrit log rt data:
crit.data$precrt<-scale(log(precritrt),scale=F)


## compare with your precrit RT:
boxplot(getprecritrt)
## this is not right:
precrit.rt <- subset(data1.uniq.reg, roi=='precrit')$rt
boxplot(precrit.rt)


#crit.data$precrit.rt <- precrit.rt
head(crit.data)


## short -1, long 1
crit.data$dist<-ifelse(crit.data$cond%in%c("a","c"),-1,1)
## CP -1, SP 1

xtabs(~cond+dist,datae2)
xtabs(~cond+dist,crit.data)

crit.data$pred<-ifelse(crit.data$cond%in%c("a","b"),-1,1)


## here is the correct analysis:
m1_spillover_crit<-lmer(log(rt)~dist*pred*precrt+
                          (dist*pred*precrt||subj)+
                          (dist*pred*precrt||item),
                        subset(crit.data,rt<3000))

## precritical region does not affect loc effect at critical:
round(summary(m1_spillover_crit)$coef,2)




############################# checking for spillover effect   SPR 2 

m2_crit_full<-lmer(log(rt)~dist*pred+(dist*pred||subj)+
                     (dist*pred||item),
                   subset(data2,roi=="crit"))
summary(m2_crit_full)


## 6/1542 extreme data points removed (0.4%):
m2_crit<-lmer(log(rt)~dist*pred+(dist*pred||subj)+
                (dist*pred||item),
              subset(data2,roi=="crit" & rt<3000))
round(summary(m2_crit)$coef,2)

library(car)
qqPlot(residuals(m2_crit))

#main effect of distance
#main effect of prediction
m2_crit<-lmer(log(rt)~dist*pred+(1|subj)+(1|item),subset(data2,roi=="postcrit"))
summary(m2_crit)

m2.nested<-lmer(log(rt)~pred+pred.dist+nopred.dist+(1|subj)+(1|item),subset(data2,roi=="crit"))
summary(m2.nested)

m2.nested<-lmer(log(rt)~pred+pred.dist+nopred.dist+
                  (pred+pred.dist+nopred.dist||subj)+
                  (pred+pred.dist+nopred.dist||item),
                subset(data2,roi=="postcrit" & rt<3000))

summary(m2.nested)

qqPlot(residuals(m2.nested))

##############################
#checking for any spillover from the precritical region
head(data2)
library(plyr)
##SV: the mistake is here:
#summing up rts across identical regions for each item
data2.uniq.reg<-ddply(data2, .(subj, item, cond, roi), summarize, rt = sum(rt))




crit.data <- subset(data2.uniq.reg, roi=='crit')

critdata<-(subset(data2,roi=="crit"))

xtabs(~item+cond,critdata)

subjlist<-unique(critdata$subj)
itemlist<-unique(critdata$item)
for(i in subjlist){
  for(j in itemlist){
    ## should always print out 1 
    print(dim(subset(critdata,item==j & subj==i))[1])
  }
}
## so there is only one region called crit in every row
## of the data frame for each subj and item. 
## The spillover region is the region preceding the critical one:

temp<-data2[!is.na(data2$roi),]
dim(temp)
unique(temp$roi=="crit")


n<-dim(temp)[1]
getprecritrt<-rep(NA,n)
## walk through data frame:
for(i in 1:n){
  if(temp[i,]$roi=="crit"){
    ## get rt before current pos:
    getprecritrt[i]<-temp[i-1,]$rt
  }
}

## get rid of NA's:
precritrt<-getprecritrt[!is.na(getprecritrt)]
## matches up:
length(precritrt)
str(precritrt)
dim(crit.data)

## center precrit log rt data:
crit.data$precrt<-scale(log(precritrt),scale=F)


boxplot(getprecritrt)
## this is not right:
precrit.rt <- subset(data2.uniq.reg, roi=='precrit')$rt
boxplot(precrit.rt)


#crit.data$precrit.rt <- precrit.rt
head(crit.data)


## short -1, long 1
crit.data$dist<-ifelse(crit.data$cond%in%c("a","c"),-1,1)
## CP -1, SP 1

xtabs(~cond+dist,datae2)
xtabs(~cond+dist,crit.data)

crit.data$pred<-ifelse(crit.data$cond%in%c("a","b"),-1,1)


## here is the correct analysis:
m2_spillover_crit<-lmer(log(rt)~dist*pred*precrt+
                          (dist*pred*precrt||subj)+
                          (dist*pred*precrt||item),
                        subset(crit.data,rt<3000))

## precritical region does not affect loc effect at critical:
round(summary(m2_spillover_crit)$coef,2)




##################################### combined analysis :
datae1$expt<-factor("e1")
datae2$expt<-factor("e2")

datae1$subj<-factor(paste(datae1$expt,datae1$subj,sep=""))
datae2$subj<-factor(paste(datae2$expt,datae2$subj,sep=""))

datae1$item<-factor(paste(datae1$expt,datae1$item,sep=""))
datae2$item<-factor(paste(datae2$expt,datae2$item,sep=""))

alldata<-rbind(datae1,datae2)

library(lme4)
m.combined<-lmer(log(rt)~pred*dist*expt+(1|subj)+(1|item),subset(alldata,rt<3000 & roi=="crit"))

summary(m.combined)
