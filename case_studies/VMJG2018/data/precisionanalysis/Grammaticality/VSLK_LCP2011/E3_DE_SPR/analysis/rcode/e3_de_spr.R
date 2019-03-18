###################################################
### chunk number 2: 
###################################################

library(reshape)
library(lme4)
library(Hmisc)
library(car)

## question accuracies:
alldataq <- read.table("../../data/qdata.txt")
colnames(alldataq) <- c("subject","expt","item","condition","dummy","answer","correct","RT")
summary(alldataq)

## remove practice items
alldataq <- subset(alldataq,expt!="practice")

## mean and se of all responses:
mean(with(alldataq,tapply(correct,subject,mean)))
se(with(alldataq,tapply(correct,subject,mean)))

alldataq.gug <- subset(alldataq,expt=="inter")
alldataq.gug$condition <- factor(alldataq.gug$condition)

## grammaticality factor:
alldataq.gug$gram <- ifelse(alldataq.gug$condition%in%c("a","b"),1,-1)

## interference factor:
alldataq.gug$int <- ifelse(alldataq.gug$condition%in%c("a","c"),1,-1)

## means and se of engug expt responses by condition:
with(alldataq.gug,tapply(correct,gram,mean,na.rm=TRUE))*100
with(alldataq.gug,tapply(correct,gram,se))*100

means.subj.gug <- (with(alldataq.gug,tapply(correct,IND=list(subject,gram),
                                            mean,na.rm=TRUE))*100)
barplot(means.subj.gug[,2]-means.subj.gug[,1])


(with(alldataq.gug,tapply(correct,IND=list(gram),
                                            mean,na.rm=TRUE))*100)
(with(alldataq.gug,tapply(correct,IND=list(gram),
                                            se))*100)

(with(alldataq.gug,tapply(correct,IND=list(int),
                                            mean,na.rm=TRUE))*100)

(with(alldataq.gug,tapply(correct,IND=list(int),
                                            se))*100)


summary(lmer(correct~gram*int+(1|subject)+(1|item),family=binomial(),alldataq.gug))


###################################################
### chunk number 3: inputdata
###################################################

data <- read.table("../../data/e3desprdata.txt")

colnames(data) <- c("subj","expt","item","condition","position","word","RT","similarity","grammaticality")

NP3.data <- subset(data,position==5)
V3.data <- subset(data,position==6)
V2.data <- subset(data,(condition%in%c("a","b") & position==7))
V1.data <- subset(data,(condition%in%c("a","b") & position==8) | (condition%in%c("c","d") & position==7))
postV1.data <- subset(data,(condition%in%c("a","b") & position==9) |
                      (condition%in%c("c","d") & position==8))

d.NP3.rs <- melt(NP3.data, id=c("subj", "condition","item","word"),
              measure="RT", variable_name="times", na.rm=TRUE)
d.NP3.rs <- subset(d.NP3.rs,value>0)

d.NP3.rs$gram <- ifelse(d.NP3.rs$condition%in%c("a","b"),"gram","ungram")
d.NP3.rs$int <- ifelse(d.NP3.rs$condition%in%c("a","c"),"hi","lo")

d.NP3.cast.i <- cast(d.NP3.rs, int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.V3.rs <- melt(V3.data, id=c("subj", "condition","item","word"),
              measure="RT", variable_name="times", na.rm=TRUE)
d.V3.rs <- subset(d.V3.rs,value>0)

d.V3.rs$gram <- ifelse(d.V3.rs$condition%in%c("a","b"),"gram","ungram")
d.V3.rs$int <- ifelse(d.V3.rs$condition%in%c("a","c"),"hi","lo")

d.V3.cast.g <- cast(d.V3.rs, gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")
d.V3.cast.i <- cast(d.V3.rs, int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.V2.rs <- melt(V2.data, id=c("subj", "condition","item","word"),
              measure="RT", variable_name="times", na.rm=TRUE)
d.V2.rs <- subset(d.V2.rs,value>0)

d.V2.rs$gram <- ifelse(d.V2.rs$condition%in%c("a","b"),"gram",NA)
d.V2.rs$int <- ifelse(d.V2.rs$condition%in%c("a"),"hi","lo")

d.V2.cast.g <- cast(d.V2.rs, gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.V2.cast.i <- cast(d.V2.rs, int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.V1.rs <- melt(V1.data, id=c("subj", "condition","item","word"),
              measure="RT", variable_name="times", na.rm=TRUE)
d.V1.rs <- subset(d.V1.rs,value>0)

d.V1.rs$gram <- ifelse(d.V1.rs$condition%in%c("a","b"),"gram","ungram")
d.V1.rs$int <- ifelse(d.V1.rs$condition%in%c("a","c"),"hi","lo")

d.V1.cast.g <- cast(d.V1.rs, gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.V1.cast.i <- cast(d.V1.rs, int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.postV1.rs <- melt(postV1.data, id=c("subj", "condition","item","word"),
              measure="RT", variable_name="times", na.rm=TRUE)
d.postV1.rs <- subset(d.postV1.rs,value>0)

d.postV1.rs$gram <- ifelse(d.postV1.rs$condition%in%c("a","b"),"gram","ungram")
d.postV1.rs$int <- ifelse(d.postV1.rs$condition%in%c("a","c"),"hi","lo")

d.postV1.cast.g <- cast(d.postV1.rs, gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.postV1.cast.i <- cast(d.postV1.rs, int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.NP3.rs <- data.frame(region="NP3",d.NP3.rs)
d.V3.rs <- data.frame(region="V3",d.V3.rs)
d.V2.rs <- data.frame(region="V2",d.V2.rs)
d.V1.rs <- data.frame(region="V1",d.V1.rs)
d.postV1.rs <- data.frame(region="postV1",d.postV1.rs)

d.rs <- rbind(d.NP3.rs,d.V3.rs,d.V2.rs,d.V1.rs,d.postV1.rs)

head(d.rs)
summary(d.rs)

#centered length
#d.V.rs$length <- center(nchar(as.character(d.V.rs$word)))

critdata <- d.rs

g <- ifelse(critdata$condition%in%c("a","b"),1,-1)
i <- ifelse(critdata$condition%in%c("a","c"),1,-1)
gxi <- ifelse(critdata$condition%in%c("a","d"),1,-1)

critdata$g <- g
critdata$i <- i
critdata$gxi <- gxi

critdata2 <- data.frame(region=critdata[,1],
                        subject=critdata[,2],
                        expt="e3",
                        item=critdata[,4],
                        condition=critdata[,3],
                        position="NA",
                        word=critdata[,5],
                        times=critdata[,6],
                        value=critdata[,7],
                        gram=critdata[,8],
                        int=critdata[,9],
                        g=critdata[,10],
                        i=critdata[,11],
                        gxi=critdata[,12],
                        lang="german",
                        method="spr")

write.table(critdata2,file="e3critdata.txt")

summary(critdata)

## comparison 0:
data0 <- subset(critdata,region=="NP3")

summary(fm0 <- lmer(log(value)~ g+i+gxi+ (1|subj)+(1|item),
                   data=data0))

## comparison 1:
data1 <- subset(critdata,region=="V3")

summary(fm1 <- lmer(log(value)~ g+i+gxi+ (1|subj)+(1|item),
                   data=data1))

print(dotplot(ranef(fm1, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subj)

print(dotplot(ranef(fm1, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$item)


## comparison 2:

data2 <- subset(critdata,(region=="V2" & condition%in%c("a","b")) |
                                        (region=="V1" & condition%in%c("c","d")))

#data2$wl <- nchar(as.character(data2$word))

#with(data2,tapply(wl,grammaticality,mean))
#with(data2,tapply(wl,grammaticality,se))

summary(fm2 <- lmer(log(value)~ g+i+gxi+
                    #center(wl)+
                    (1|subj),#+(1|item),
                    data=data2))


print(dotplot(ranef(fm2, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subj)

## comparison 3:
data3 <- subset(critdata,(region=="V1"))

summary(fm3 <- lmer(log(value)~ g+i+gxi +(1|subj)+(1|item),
                   data=subset(data3,value<2000)))

summary(fm3 <- lmer(value~ g+i+gxi +(1|subj)+(1|item),
                   data=subset(data3,value<2000)))


print(dotplot(ranef(fm3, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subj)

print(dotplot(ranef(fm3, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$item)


## comparison 4:
data4 <- subset(critdata,(region=="postV1"))

summary(fm4 <- lmer(log(value)~ g+i+gxi+(1|subj)+(1|item),
                   data=subset(data4,value<2000)))

summary(fm4 <- lmer(value~ g+i+gxi+(1|subj)+(1|item),
                   data=subset(data4,value<2000)))



print(dotplot(ranef(fm4, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subj)

print(dotplot(ranef(fm4, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$item)



qq.plot(residuals(fm1))
qq.plot(residuals(fm2))
qq.plot(residuals(fm3))
qq.plot(residuals(fm4))

## save data for meta-analysis
#write(t(critdata),ncolumns=10,file="e1ensprcrit.txt")

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
           coef.fm3,coef.fm4)

hpds <- rbind(hpd.fm0$fixef[1:4,],
              hpd.fm1$fixef[1:4,],
#              hpd.fm2$fixef[1:4,], ## no need to plot this
              hpd.fm3$fixef[1:4,],
              hpd.fm4$fixef[1:4,])

g.hpd <- hpds[c(2,6,10,14),]
i.hpd <- hpds[c(2,6,10,14)+1,]

coefs.plot <- cbind(coefs[c(2,6,10,14)],g.hpd)


library(xtable)

xtable(coefs.plot,digits=5)


verbrts.g <- rbind(d.V3.cast.g,
                   d.V2.cast.g,
                   c("ungram","NA","NA","NA"),
                   d.V1.cast.g,
                 d.postV1.cast.g)

verbrts.g$verb <- rep(c("V3","V2","V1","Post-V1"),each=2)
verbrts.g$region <- rep(1:4,each=2)
names(verbrts.g$region) <- "Region"
levels(verbrts.g$region) <- verbrts.g$verb

verbrts.g$ci.upper <- as.numeric(as.character(verbrts.g$ci.upper))
verbrts.g$ci.lower <- as.numeric(as.character(verbrts.g$ci.lower))
verbrts.g$M <- as.numeric(as.character(verbrts.g$M))

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




## main graphic with matplot
gram.means <- subset(verbrts.g,gram=="gram")
ungram.means <- subset(verbrts.g,gram=="ungram")

graycol <- gray(.5)

filename <- "e3desprplotgram.ps"
createPS(filename)

matplot(gram.means$M,
        ylim=c(min(verbrts.g$ci.lower,na.rm=T),
               max(verbrts.g$ci.upper,na.rm=T)),
        main="Experiment 3 (German SPR)",
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
mtext(text="Reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)

legend(2,2150,lty=c(1,2),col=c("black",graycol),pch=c(16,19),
       legend=c("grammatical","ungrammatical"),cex=2,lwd=3)

dev.off()

destination <- "~/Dropbox/Papers/IntML/Figures"
system(paste("cp ",filename,destination,sep=" "))


hi.means <- subset(verbrts.i,int=="hi")
lo.means <- subset(verbrts.i,int=="lo")

filename <- "e3desprplotint.ps"
createPS(filename)

matplot(hi.means$M,
        ylim=c(min(verbrts.i$ci.lower,na.rm=T),
               max(verbrts.i$ci.upper,na.rm=T)),
        main="Experiment 3 (German SPR): interference",
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

legend(2.2,2000,lty=c(1,2),col=c("black",graycol),pch=c(16,19),
       legend=c("high interference","low interference"),cex=2,lwd=3)

dev.off()

destination <- "~/Documents/SVPapers/InProgress/IntML/Figures"
system(paste("cp ",filename,destination,sep=" "))




