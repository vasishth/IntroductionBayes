library(reshape)
library(lattice)

library(lme4)
library(Hmisc)
library(car)
set.seed(123454321)

load(file="../../Data/degug1Eye.Rda")

d.rs$gram <- ifelse(d.rs$condition%in%c("a","b"),"gram","ungram")
d.rs$int <- ifelse(d.rs$condition%in%c("a","c"),"hi","lo")
d.rs <- subset(d.rs,times=="RRT")
d.rs$times <- factor(d.rs$times)
d.rs$gram <- factor(d.rs$gram)
d.rs$int <- factor(d.rs$int)

d.rs <- subset(d.rs,value>0)

d.rs$value <- d.rs$value/1000

NP3.data <- subset(d.rs,roi==5)
d.NP3.rs <- NP3.data ## already melted
d.NP3.cast.g <- cast(d.NP3.rs, gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")
d.NP3.cast.i <- cast(d.NP3.rs, int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")

V3.data <- subset(d.rs,roi==9)
d.V3.rs <- V3.data ## already melted

d.V3.cast.g <- cast(subset(d.V3.rs,value<2000), gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")
d.V3.cast.i <- cast(subset(d.V3.rs,value<2000), int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")

V2.data <- subset(d.rs,(condition%in%c("a","b") & roi==10))
d.V2.rs <- V2.data ## already melted
d.V2.cast.g <- cast(subset(d.V2.rs,value<2000), gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")
d.V2.cast.i <- cast(subset(d.V2.rs,value<2000), int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")

V1.data <- subset(d.rs,((condition%in%c("a","b") & roi==11) |
                        (condition%in%c("c","d") & roi==10)))
d.V1.rs <- V1.data ## already melted
d.V1.cast.g <- cast(subset(d.V1.rs,value<2000), gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")
d.V1.cast.i <- cast(subset(d.V1.rs,value<2000), int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")

postV1.data <- subset(d.rs,((condition%in%c("a","b") & roi==12) |
                        (condition%in%c("c","d") & roi==11)))
d.postV1.rs <- postV1.data ## already melted
d.postV1.cast.g <- cast(subset(d.postV1.rs,value<2000), gram ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")
d.postV1.cast.i <- cast(subset(d.postV1.rs,value<2000), int ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RRT")

d.NP3.rs <- data.frame(region="NP3",d.V3.rs)
d.V3.rs <- data.frame(region="V3",d.V3.rs)
d.V2.rs <- data.frame(region="V2",d.V2.rs)
d.V1.rs <- data.frame(region="V1",d.V1.rs)
d.postV1.rs <- data.frame(region="postV1",d.postV1.rs)

d.V.rs <- rbind(d.NP3.rs,d.V3.rs,d.V2.rs,d.V1.rs,d.postV1.rs)


critdata <- d.V.rs

g <- ifelse(critdata$condition%in%c("a","b"),1,-1)
i <- ifelse(critdata$condition%in%c("a","c"),1,-1)
gxi <- ifelse(critdata$condition%in%c("a","d"),1,-1)

critdata$g <- g
critdata$i <- i
critdata$gxi <- gxi

critdata2 <- data.frame(region=critdata[,1],
                        subject=critdata[,2],
                        expt="e4",
                        item=critdata[,10],
                        condition=critdata[,7],
                        position=critdata[,6],
                        word="NA",
                        times=critdata[,11],
                        value=critdata[,12],
                        gram=critdata$gram,
                        int=critdata$int,
                        g=critdata$g,
                        i=critdata$i,
                        gxi=critdata$gxi,
                        lang="german",
                        method="et"
                        )

write.table(critdata2,file="e4critdata.txt")

## comparison 0:
data0 <- subset(critdata,region=="NP3")

summary(fm0 <- lmer(log(value)~ g + i + gxi + (1|subject)+(1|itemnum),
                   data=data0))

## comparison 1:
data1 <- subset(critdata,region=="V3")

summary(fm1 <- lmer(log(value)~ g + i + gxi + (1|subject)+(1|itemnum),
                   data=data1))

## comparison 2:
data2 <- subset(critdata,(region=="V2" & gram=="gram") |
                                        (region=="V1" & gram=="ungram"))

#data2$wl <- nchar(as.character(data2$word))

#with(data2,tapply(wl,grammaticality,mean))
#with(data2,tapply(wl,grammaticality,se))

summary(fm2 <- lmer(log(value)~ g + i + gxi +(1|subject)+(1|itemnum),
                   data=data2))

## comparison 3:
data3 <- subset(critdata,(region=="V1"))
summary(fm3 <- lmer(log(value)~ g+i+gxi +(1|subject)+(1|itemnum),
                   data=subset(data3,value<2000)))

summary(fm3 <- lmer(value~ g+i+gxi +(1|subject)+(1|itemnum),
                   data=subset(data3,value<2000)))



## comparison 4:
data4 <- subset(critdata,(region=="postV1"))

summary(fm4 <- lmer(log(value)~ g+i+gxi +(1|subject)+(1|itemnum),
                   data=subset(data4,value<2000)))

summary(fm4 <- lmer(value~ g+i+gxi +(1|subject)+(1|itemnum),
                   data=subset(data4,value<2000)))

summary(fm4 <- lmer(value~ g+i+gxi +(1|subject)+(1|itemnum),
                   data=data4))




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
coefs.plot.i <- cbind(coefs[c(2,6,10,14)],i.hpd)


### 
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

filename <- "e4deetplotgram.ps"
createPS(filename)

matplot(gram.means$M,
        ylim=c(min(verbrts.g$ci.lower,na.rm=T),
               max(verbrts.g$ci.upper,na.rm=T)+200),
        main="Experiment 4 (German Eyetracking)",
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
mtext(text="Re-Reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)

legend(2,1230,lty=c(1,2),col=c("black",graycol),pch=c(16,19),
       legend=c("grammatical","ungrammatical"),cex=2,lwd=3)

dev.off()

destination <- "~/Dropbox/Papers/IntML/Figures"
system(paste("cp ",filename,destination,sep=" "))


hi.means <- subset(verbrts.i,int=="hi")
lo.means <- subset(verbrts.i,int=="lo")

filename <- "e4deetplotint.ps"
createPS(filename)

matplot(hi.means$M,
        ylim=c(min(verbrts.i$ci.lower,na.rm=T),
               max(verbrts.i$ci.upper,na.rm=T)),
        main="Experiment 4 (German eyetracking): interference",
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

legend(2.2,1400,lty=c(1,2),col=c("black",graycol),pch=c(16,19),
       legend=c("high interference","low interference"),cex=2,lwd=3)

dev.off()

destination <- "~/Documents/SVPapers/InProgress/IntML/Figures"
system(paste("cp ",filename,destination,sep=" "))

###






## V3 vs V2 in grammatical conditions
summary(v3v2.lmer <- lmer(value~factor(verb)+(1|subject)+(1|itemnum),
             subset(d.V.rs,(gram=="gram" & verb!="V1"))))

qq.plot(residuals(v3v2.lmer))

## V2 vs V1 in grammatical conditions
summary(v2v1.lmer <- lmer(value~factor(verb)+(1|subject)+(1|itemnum),
             subset(d.V.rs,(gram=="gram" & verb!="V3"))))

qq.plot(residuals(v2v1.lmer))

## V2 gram vs V1 ungram
summary(v2v1.lmer <- lmer(value~factor(verb)+(1|subject)+(1|itemnum),
             subset(d.V.rs,(gram=="gram" & verb=="V2") |
                    (gram=="ungram" & verb=="V1"))))

qq.plot(residuals(v2v1.lmer))

summary(v1.lmer <- lmer(value~factor(gram)+(1|subject)+(1|itemnum),
             subset(d.V.rs,verb=="V1")))

qq.plot(residuals(v1.lmer))

verbmeans <- rbind(d.V3.cast,d.V2.cast,d.V1.cast)

xtable(verbmeans)


## question response accuracies

qdata <- subset(d.rs,roi==1)
dim(qdata) ## num rows: 51 subjects x 12 items = 612

summary(qdata)

with(qdata,tapply(response,IND=list(gram),mean))
with(qdata,tapply(response,IND=list(int),mean))

g <- ifelse(qdata$condition%in%c("a","b"),1,-1)
i <- ifelse(qdata$condition%in%c("a","c"),1,-1)
gxi <- ifelse(qdata$condition%in%c("a","d"),1,-1)

qdata$g <- g
qdata$i <- i
qdata$gxi <- gxi

summary(lmer(response~g+i+gxi+(1|subject)+(1|itemnum),family=binomial(),qdata))

length(unique(qdata$subject))
length(unique(qdata$itemnum))
dim(qdata)
