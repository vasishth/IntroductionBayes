library(reshape)
library(lattice)
library(lme4)
library(Hmisc)
library(car)
set.seed(123454321)

## data for computing all data means, including non-critical (nc) sentences
data.nc <- read.table("../../data/e2_en_et_ncdata.txt",header=TRUE)

## mean response accuracy all items
mean(with(subset(data.nc,roi==1),tapply(accuracy,condition,mean,na.rm=TRUE)))

alldataq.gug <- subset(data.nc,experiment=="gug")

alldataq.gug$condition <- factor(alldataq.gug$condition)

## code up grammaticality factor:
alldataq.gug$gram <- ifelse(alldataq.gug$condition%in%c("a","b"),1,-1)
alldataq.gug$g <- ifelse(alldataq.gug$condition%in%c("a","b"),"gram","ungram")

## code up interference factor:
alldataq.gug$int <- ifelse(alldataq.gug$condition%in%c("a","c"),1,-1)
alldataq.gug$i <- ifelse(alldataq.gug$condition%in%c("a","c"),"hi","lo")

dq.rs <- melt(alldataq.gug, id=c("subject", "condition","item","roi","gram"),
              measure="accuracy", variable_name="response", na.rm=TRUE)

summary(subset(alldataq.gug,roi==5))

head(subset(alldataq.gug,roi==5))

is.numeric(subset(alldataq.gug,roi==5)$accuracy)

is.data.frame(subset(alldataq.gug,roi==5))

with(subset(alldataq.gug,roi==5),tapply(accuracy,gram,mean))
with(subset(alldataq.gug,roi==5),tapply(accuracy,int,mean))

summary(lmer(accuracy~gram*int+(1|subject)+(1|item),family=binomial(),
             subset(alldataq.gug,roi==5)))

ftable(xtabs(accuracy~accuracy+condition+item+subject,subset(alldataq.gug,roi==5)))

## fixation duration data:
data <- read.table("../../data/e2_en_et_data.txt",header=TRUE)

## item 11 is bad for verb 3
data <- subset(data,item!=11)

data$g <- ifelse(data$condition%in%c("a","b"),
                    1, -1)
data$i <- ifelse(data$condition%in%c("a","c"),
                    1, -1)

data$gram <- ifelse(data$condition%in%c("a","b"),
                    "gram", "ungram")
data$int <- ifelse(data$condition%in%c("a","c"),
                    "hi", "lo")

NP3.data <- subset(data,roi==5) ## this is the det+N
d.NP3.rs <- melt(NP3.data, id=c("subject", "condition","item","roi","gram","int","g","i"),
              measure="RRT", variable_name="times", na.rm=TRUE)

d.NP3.rs <- subset(d.NP3.rs,value>0)

d.NP3.cast.g <- cast(d.NP3.rs, gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")
d.NP3.cast.i <- cast(d.NP3.rs, int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")

V3.data <- subset(data,roi==9)
d.V3.rs <- melt(V3.data, id=c("subject", "condition","item","roi","gram","int","g","i"),
              measure="RRT", variable_name="times", na.rm=TRUE)

d.V3.rs <- subset(d.V3.rs,value>0)

d.V3.cast.g <- cast(subset(d.V3.rs,value<2000), gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")
d.V3.cast.i <- cast(subset(d.V3.rs,value<2000), int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")


V2.data <- subset(data,(condition%in%c("a","b") & roi==10))
d.V2.rs <- melt(V2.data, id=c("subject", "condition","item","roi","gram","int","g","i"),
              measure="RRT", variable_name="times", na.rm=TRUE)

d.V2.rs <- subset(d.V2.rs,value>0)

d.V2.cast.g <- cast(subset(d.V2.rs,value<2000), gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")
d.V2.cast.i <- cast(subset(d.V2.rs,value<2000), int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")

V1.data <- subset(data,((condition%in%c("a","b") & roi==11) |
                        (condition%in%c("c","d") & roi==10)))
d.V1.rs <- melt(V1.data, id=c("subject", "condition","item","roi","gram","int","g","i"),
              measure="RRT", variable_name="times", na.rm=TRUE)
d.V1.rs <- subset(d.V1.rs,value>0)

d.V1.cast.g <- cast(subset(d.V1.rs,value<2000), gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")
d.V1.cast.i <- cast(subset(d.V1.rs,value<2000), int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")

postV1.data <- subset(data,((condition%in%c("a","b") & roi==12) |
                        (condition%in%c("c","d") & roi==11)))
d.postV1.rs <- melt(postV1.data,
                    id=c("subject", "condition","item","roi","gram","int","g","i"),
              measure="RRT", variable_name="times", na.rm=TRUE)

d.postV1.rs <- subset(d.postV1.rs,value>0)

d.postV1.cast.g <- cast(subset(d.postV1.rs,value<2000), gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")
d.postV1.cast.i <- cast(subset(d.postV1.rs,value<2000), int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")

d.NP3.rs <- data.frame(region="NP3",d.NP3.rs)
d.V3.rs <- data.frame(region="V3",d.V3.rs)
d.V2.rs <- data.frame(region="V2",d.V2.rs)
d.V1.rs <- data.frame(region="V1",d.V1.rs)
d.postV1.rs <- data.frame(region="Post-V1",d.postV1.rs)

d.V.rs <- rbind(d.NP3.rs,d.V3.rs,d.V2.rs,d.V1.rs,d.postV1.rs)

critdata <- d.V.rs

critdata2 <- data.frame(region=critdata[,1],
                        subject=critdata[,2],
                        expt="e2",
                        item=critdata[,4],
                        condition=critdata[,3],
                        position=critdata[,5],
                        word="NA",
                        times=critdata[,10],
                        value=critdata[,11],
                        gram=critdata[,6],
                        int=critdata[,7],
                        g=critdata[,8],
                        i=critdata[,9],
                        gxi=(critdata[,8]*critdata[,9]),
                        lang="english",
                        method="et")

write.table(critdata2,file="e2critdata.txt")

## already done:
#g <- ifelse(critdata$condition%in%c("a","b"),-1,1)
#i <- ifelse(critdata$condition%in%c("a","c"),-1,1)
#gxi <- ifelse(critdata$condition%in%c("a","d"),1,-1)

#critdata$g <- g
#critdata$i <- i
#critdata$gxi <- gxi

## comparison 0:
data0 <- subset(critdata,region=="NP3")

summary(fm0 <- lmer(log(value)~ g*i+(1|subject),
                   data=data0))

summary(fm0 <- lmer(log(value)~ g*i+(1|subject),
                   data=subset(data0,value>0)))

## with RRT including 0
summary(fm0zero <- lmer(value~ g*i + (1|subject)+(1|item),
                   data=data0))

library(car)

qq.plot(residuals(fm0zero))

## comparison 1:
data1 <- subset(critdata,region=="V3")


print(dotplot(ranef(fm1, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subject)

print(dotplot(ranef(fm1, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$item)


## comparison 2:

data2 <- subset(critdata,(region=="V2" & gram=="gram") |
                                        (region=="V1" & gram=="ungram"))

summary(fm2 <- lmer(log(value)~ g*i +(1|subject)+(1|item),
                   data=data2))

print(dotplot(ranef(fm2, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subject)

print(dotplot(ranef(fm2, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$item)


## comparison 3:
data3 <- subset(critdata,(region=="V1"))

summary(fm3 <- lmer(log(value)~ g*i+(1|subject)+(1|item),
                   data=subset(data3,value<2000)))

summary(fm3 <- lmer(value~ g*i+(1|subject)+(1|item),
                   data=subset(data3,value<2000)))

print(dotplot(ranef(fm3, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subject)

print(dotplot(ranef(fm3, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$item)


## comparison 4:
data4 <- subset(critdata,(region=="Post-V1"))

summary(fm4 <- lmer(log(value)~ g*i+(1|subject)+(1|item),
                   data=subset(data4,value<2000)))

summary(fm4 <- lmer(value~ g*i+(1|subject)+(1|item),
                   data=subset(data4,value<2000)))



summary(fm4 <- lmer(log(value)~ g*i+(1|subject)+(1|item),
                   data=subset(data4)))

print(dotplot(ranef(fm4, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subject)

print(dotplot(ranef(fm4, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$item)



qq.plot(residuals(fm1))
qq.plot(residuals(fm2))
qq.plot(residuals(fm3))
qq.plot(residuals(fm4))


## save data for meta-analysis
write(t(critdata),ncolumns=10,file="e2enetcrit.txt")

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
coef.fm2 <- fixef(fm2)
coef.fm3 <- fixef(fm3)
coef.fm4 <- fixef(fm4)

coefs <- c(coef.fm0,
           coef.fm1,
           #coef.fm2,
           coef.fm3,
           coef.fm4)

hpds <- rbind(hpd.fm0$fixef[1:4,],
              hpd.fm1$fixef[1:4,],
#              hpd.fm2$fixef[1:4,], ## no need to plot this
              hpd.fm3$fixef[1:4,],
              hpd.fm4$fixef[1:4,])

g.hpd <- hpds[c(2,6,10,14),]
i.hpd <- hpds[c(2,6,10,14)+1,]

coefs.plot.g <- cbind(coefs[c(2,6,10,14)],g.hpd)
coefs.plot.i <- cbind(coefs[c(2,6,10,14)+1],i.hpd)

library(xtable)
xtable(coefs.plot,digits=5)

source("~/Data/functions/plotcoefs.R")

createPS("e2enetHPD.ps")

plotcoefs(coefs.plot,p=3)

dev.off()

verbrts.g <- rbind(d.V3.cast.g,
                 d.V2.cast.g,
                 c("ungram","NA","NA","NA"),
                 d.V1.cast.g,
                 d.postV1.cast.g)

verbrts.g$verb <- rep(c("V3","V2","V1","Post-V1"),each=2)
verbrts.g$region <- rep(1:4,each=2)
names(verbrts.g$region) <- "Region"
levels(verbrts.g$region) <- verbrts.g$verb
verbrts.g$ci.upper <- as.numeric(verbrts.g$ci.upper)
verbrts.g$ci.lower <- as.numeric(verbrts.g$ci.lower)
verbrts.g$M <- as.numeric(verbrts.g$M)

verbrts.i <- rbind(d.NP3.cast.i,
                   d.V3.cast.i,
                   d.V2.cast.i,
                   d.V1.cast.i,
                   d.postV1.cast.i)

verbrts.i$verb <- rep(c("NP3","V3","V2","V1","Post-V1"),each=2)
verbrts.i$region <- rep(1:5,each=2)
names(verbrts.i$region) <- "Region"
levels(verbrts.i$region) <- verbrts.i$verb
verbrts.i$ci.upper <- as.numeric(verbrts.i$ci.upper)
verbrts.i$ci.lower <- as.numeric(verbrts.i$ci.lower)
verbrts.i$M <- as.numeric(verbrts.i$M)


gram.means <- subset(verbrts.g,gram=="gram")
ungram.means <- subset(verbrts.g,gram=="ungram")

graycol <- gray(.5)

filename <- "e2enetplotgram.ps"
createPS(filename)

matplot(gram.means$M,
        ylim=c(min(verbrts.g$ci.lower,na.rm=T),
               max(verbrts.g$ci.upper,na.rm=T)+200),
        main="Experiment 2 (English Eyetracking)",
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

arrows(seq(from=1,4,by=1),gram.means$ci.lower,
       seq(from=1,4,by=1),gram.means$ci.upper,
       lwd =  2,col="black",angle=90,code=3,length=.1)

arrows(seq(from=1,4,by=1),ungram.means$ci.lower,
       seq(from=1,4,by=1),ungram.means$ci.upper,
       lwd =  2,col=graycol,angle=90,code=3,length=.1)

mtext(text=c("V3","V2","V1","Post-V1"),side=1,line=.75,at=c(1,2,3,3.9),cex=2)
mtext(text="Re-reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)

legend(2,1100,lty=c(1,2),col=c("black",graycol),pch=c(16,19),
       legend=c("grammatical","ungrammatical"),cex=2,lwd=3)

dev.off()

destination <- "~/Dropbox/Papers/IntML/Figures"
system(paste("cp ",filename,destination,sep=" "))


hi.means <- subset(verbrts.i,int=="hi")
lo.means <- subset(verbrts.i,int=="lo")

filename <- "e2enetplotint.ps"
createPS(filename)

matplot(hi.means$M,
        ylim=c(min(verbrts.i$ci.lower,na.rm=T),
               max(verbrts.i$ci.upper,na.rm=T)+300),
        main="Experiment 2 (English eyetracking): interference",
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

arrows(seq(from=1,5,by=1),hi.means$ci.lower,
       seq(from=1,5,by=1),hi.means$ci.upper,
       lwd =  2,col="black",angle=90,code=3,length=.1)

arrows(seq(from=1,5,by=1),lo.means$ci.lower,
       seq(from=1,5,by=1),lo.means$ci.upper,
       lwd =  2,col=graycol,angle=90,code=3,length=.1)

mtext(text=c("NP3","V3","V2","V1","Post-V1"),side=1,line=.75,at=c(1,2,3,4,4.9),cex=2)
mtext(text="Reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)

legend(2.2,1500,lty=c(1,2),col=c("black",graycol),pch=c(16,19),
       legend=c("high interference","low interference"),cex=2,lwd=3)

dev.off()

destination <- "~/Documents/SVPapers/InProgress/IntML/Figures"
system(paste("cp ",filename,destination,sep=" "))


