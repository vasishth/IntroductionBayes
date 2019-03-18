###################################################
### chunk number 2: 
###################################################

library(reshape)
library(lme4)
library(Hmisc)
library(car)

## question accuracies:
alldata <- read.table("../../data/qdata.txt")
colnames(alldata) <- c("subject","expt","item","condition","dummy","answer","correct","RT")
summary(alldata)


###################################################
### chunk number 3: inputdata
###################################################

data <- read.table("../../data/e3desprdata.txt")

colnames(data) <- c("subj","expt","item","condition","position","word","RT","similarity","grammaticality")

V3.data <- subset(data,position==6)
V2.data <- subset(data,(condition%in%c("a","b") & position==7))
V1.data <- subset(data,(condition%in%c("a","b") & position==8) | (condition%in%c("c","d") & position==7))
postV1.data <- subset(data,(condition%in%c("a","b") & position==9) |
                      (condition%in%c("c","d") & position==8))

d.V3.rs <- melt(V3.data, id=c("subj", "condition","item","word","grammaticality"),
              measure="RT", variable_name="times", na.rm=TRUE)
d.V3.rs <- subset(d.V3.rs,value>0)
d.V3.cast <- cast(d.V3.rs, grammaticality ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.V2.rs <- melt(V2.data, id=c("subj", "condition","item","word","grammaticality"),
              measure="RT", variable_name="times", na.rm=TRUE)
d.V2.rs <- subset(d.V2.rs,value>0)
d.V2.cast <- cast(d.V2.rs, grammaticality ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.V1.rs <- melt(V1.data, id=c("subj", "condition","item","word","grammaticality"),
              measure="RT", variable_name="times", na.rm=TRUE)
d.V1.rs <- subset(d.V1.rs,value>0)
d.V1.cast <- cast(d.V1.rs, grammaticality ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.postV1.rs <- melt(postV1.data, id=c("subj", "condition","item","word","grammaticality"),
              measure="RT", variable_name="times", na.rm=TRUE)
d.postV1.rs <- subset(d.postV1.rs,value>0)
d.postV1.cast <- cast(d.postV1.rs, grammaticality ~ ., function(x) 
               c(M=round(mean(x)), ci=round(ci(x))), 
               subset=times=="RT")

d.V3.rs <- data.frame(verb="V3",d.V3.rs)
d.V2.rs <- data.frame(verb="V2",d.V2.rs)
d.V1.rs <- data.frame(verb="V1",d.V1.rs)
d.postV1.rs <- data.frame(verb="postV1",d.postV1.rs)


d.V.rs <- rbind(d.V3.rs,d.V2.rs,d.V1.rs,d.postV1.rs)

head(d.V.rs)

#centered length
#d.V.rs$length <- center(nchar(as.character(d.V.rs$word)))

critdata <- d.V.rs

g <- ifelse(critdata$condition%in%c("a","b"),-1,1)
i <- ifelse(critdata$condition%in%c("a","c"),-1,1)
gxi <- ifelse(critdata$condition%in%c("a","d"),-1,1)

critdata$g <- g
critdata$i <- i
critdata$gxi <- gxi


## comparison 1:
data1 <- subset(critdata,verb=="V3")

summary(fm1 <- lmer(log(value)~ g+i+gxi+ (1|subj)+(1|item),
                   data=data1))

## comparison 2:

data2 <- subset(critdata,(verb=="V2" & grammaticality=="grammatical") |
                                        (verb=="V1" & grammaticality=="ungrammatical"))

#data2$wl <- nchar(as.character(data2$word))

#with(data2,tapply(wl,grammaticality,mean))
#with(data2,tapply(wl,grammaticality,se))

summary(fm2 <- lmer(log(value)~ g+i+gxi+
                    #center(wl)+
                    (1|subj),#+(1|item),
                    data=data2))

## comparison 3:
data3 <- subset(critdata,(verb=="V1"))
summary(fm3 <- lmer(log(value)~ g+i+gxi +(1|subj)+(1|item),
                   data=data3))

## comparison 4:
data4 <- subset(critdata,(verb=="postV1"))
summary(fm4 <- lmer(log(value)~ g+i+gxi+(1|subj)+(1|item),
                   data=data4))




qq.plot(residuals(fm1))
qq.plot(residuals(fm2))
qq.plot(residuals(fm3))
qq.plot(residuals(fm4))




verbmeans <- rbind(d.V3.cast,d.V2.cast,c("ungram","NA","NA","NA"),d.V1.cast,d.postV1.cast)
colnames(verbmeans) <- c("gram","M","CI.lower","CI.upper")
verbmeans$gram <- rep(c("gram","ungram"),4)

xtable(verbmeans)

verbRTs <- verbmeans

verbRTs$verb <- rep(c("V3","V2","V1","Post-V1"),each=2)
verbRTs$region <- rep(1:4,each=2)
names(verbRTs$region) <- "Region"
levels(verbRTs$region) <- verbRTs$verb

verbRTs$CI.upper <- as.numeric(verbRTs$CI.upper)
verbRTs$CI.lower <- as.numeric(verbRTs$CI.lower)
verbRTs$M <- as.numeric(verbRTs$M)


labmag <- 2

  mykey <- list(
  #rectangles = list(col= c(rev(heat.colors(5))[1:5],
  #rev(heat.colors(5))[4:1])),
#  title="Comparisons",
  space="right",
  text = list(lab = rev(c("1", "2")),cex=2),
  lty=1:2,              
  columns = 1) 

min <- min(verbRTs$CI.lower,na.rm=TRUE)-50
max <- max(verbRTs$CI.upper,na.rm=TRUE)+50

createPS("expt3verbs.ps")

print(xYplot(Cbind(M,CI.lower,CI.upper)~as.numeric(region),verbRTs,groups=gram,
       layout=c(1,1),
       lty=1:2,
       type="b",
       lwd=3,
       pch=c(16,1),
       ylim=range(c(min,max)),
       label.curves=FALSE,
       xlab=list("Region",cex=labmag),
       scales = list(x = list(at=seq(1,4, by=1),labels=c("V3","V2","V1","Post-V1"),
                       cex=labmag),
       y=list(cex=labmag/1.5)),
       main=list("German SPR Expt. 3",cex=labmag),
       ylab=list("Reading time [ms]",cex=labmag),
       ))

dev.off()


