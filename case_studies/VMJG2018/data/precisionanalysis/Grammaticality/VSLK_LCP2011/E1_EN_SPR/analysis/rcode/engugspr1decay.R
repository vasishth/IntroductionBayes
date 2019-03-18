## This file contains the non-public version of the data analyses for
##  Expt 1 presented in the paper:
#   Shravan Vasishth, Katja Suckow, Richard Lewis, and Sabine Kern.
#   Short-term forgetting in sentence comprehension:
#   Crosslinguistic evidence from head-final structures.
#   Submitted to Language and Cognitive Processes, 2007.


library(lme4)
library(car)
library(coda)
library(Hmisc)

## load data and prepare:

## question responses
alldataq <- read.table("../../data/allquestions.txt")
colnames(alldataq) <- c("subject","expt","item","condition","question","response","correct","qrt")

## remove practice items
alldataq <- subset(alldataq,expt!="practice")

## mean and se of all responses:
mean(with(alldataq,tapply(correct,subject,mean)))
se(with(alldataq,tapply(correct,subject,mean)))

alldataq.gug <- subset(alldataq,expt=="gug")
alldataq.gug$condition <- factor(alldataq.gug$condition)

## code up grammaticality factor:
alldataq.gug$gram <- ifelse(alldataq.gug$condition%in%c("a","b"),0,1)

## means and se of engug expt responses by condition:
with(alldataq.gug,tapply(correct,gram,mean,na.rm=TRUE))*100
with(alldataq.gug,tapply(correct,gram,se))*100

means.subj.gug <- (with(alldataq.gug,tapply(correct,IND=list(subject,gram),mean,na.rm=TRUE))*100)
barplot(means.subj.gug[,2]-means.subj.gug[,1])

summary(lmer(correct~gram+(1|item),family=binomial(),alldataq.gug))


###

data <- read.table("data2.txt")
colnames(data) <- c("subj","expt","item","condition","position","word","RT")

data$position <- as.numeric(data$position)+1

dataq <- read.table("engugspr1q.txt")
colnames(dataq) <- c("subj","expt","item","condition","dummy","response","correct","RT")

data.a.uni <- subset(data,condition=="a")
data.b.uni <- subset(data,condition=="b")
data.c.uni <- subset(data,condition=="c")
data.d.uni <- subset(data,condition=="d")

data.ab.uni <- rbind(data.a.uni,data.b.uni)
data.cd.uni <- rbind(data.c.uni,data.d.uni)


library(reshape)

## computations cached:
d.rs <- melt(data, id=colnames(data),
                measure="RT", variable_name="times",
                na.rm=TRUE)


## recode regions of interest:

## The painter who the film that the friend liked admired the poet.  
##  1    2      3   4   5   6     7   8     9     10      11  12
##    1         2     3     4       5 

## recode the regions of interest:
d.rs$roi <- ifelse(d.rs$position==2,1, # NP1
               ifelse(d.rs$position==3,2, # who
                 ifelse(d.rs$position%in%c(4,5),3, #NP2
                            ifelse(d.rs$position==6,4, # that
                                   ifelse(d.rs$position%in%c(7,8),5, # NP3
                                                 d.rs$position)))))

## V3:

pos09data <- subset(d.rs,position==9) ## had
pos10data <- subset(d.rs,position==10) 

pos10data$pos09RT <- pos09data$RT
pos10data$sumRT <- pos10data$value+pos10data$pos09RT
pos10data$meanRT <- (pos10data$value+pos10data$pos09RT)/2

d.rs.V3 <- melt(pos10data, id=colnames(pos10data)[c(1,2,3,4,5,6)],
                measure=c("sumRT","meanRT"), variable_name="times",
                na.rm=FALSE)

d.rs.V3$gram <- ifelse(d.rs.V3$condition%in%c("a","b"),"gram","ungram")

d.cast.V3.sumRT   <- cast(d.rs.V3, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="sumRT")

d.cast.V3.meanRT   <- cast(d.rs.V3, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="meanRT")

## V2

pos15data <- subset(data.ab.uni,position==15) ## V2 in a,b
auxRTs <- subset(data.ab.uni,position==14)$RT ## auxiliary RTs
pos15data$sumRT <- pos15data$RT+auxRTs
pos15data$meanRT <- (pos15data$RT+auxRTs)/2

d.rs.V2ab <- melt(pos15data, id=colnames(pos15data)[c(1:6)],
                measure="sumRT", variable_name="times",
                preserve.na=TRUE)

d.rs.V2ab$gram <- ifelse(d.rs.V2ab$condition%in%c("a","b"),"gram","ungram")

d.cast.V2ab   <- cast(d.rs.V2ab, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="sumRT")


## V1:

pos17data <- subset(data.ab.uni,position==17)
pos14data <- subset(data.cd.uni,position==14)
pos1714data <- rbind(pos17data,pos14data)

d.rs.V1 <- melt(pos1714data, id=colnames(pos1714data)[1:6],
                measure="RT", variable_name="times",
                na.rm=TRUE)

d.rs.V1$gram <- ifelse(d.rs.V1$condition%in%c("a","b"),"gram","ungram")

d.cast.V1   <- cast(d.rs.V1, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="RT")

## Post V1:

pos18data <- subset(data.ab.uni,position==18)
pos15data <- subset(data.cd.uni,position==15)
pos1815data <- rbind(pos18data,pos15data)

d.rs.postV1 <- melt(pos1815data, id=colnames(pos1815data)[1:6],
                measure="RT", variable_name="times",
                na.rm=TRUE)

d.rs.postV1$gram <- factor(ifelse(d.rs.postV1$condition%in%c("a","b"),"gram","ungram"))

d.cast.postV1   <- cast(d.rs.postV1, gram ~ .,
                 function(x) c(M=round(mean(x)),
                               CI=round(ci(x))), subset=times=="RT")


verbRTs <- rbind(d.cast.V3.sumRT,d.cast.V2ab,c("ungram","NA","NA","NA"),d.cast.V1,d.cast.postV1)

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

createPS("expt1verbs.ps")
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
       main=list("English SPR Expt. 1",cex=labmag),
       ylab=list("Reading time [ms]",cex=labmag),
       ))
dev.off()

xtable(verbRTs)

d.rs.V3 <- subset(d.rs.V3,times=="sumRT")

d.rs.V3$gram <- factor(d.rs.V3$gram) 
d.rs.V3$times <- factor(d.rs.V3$times) 

d.rs.V3 <- data.frame(verb="V3",d.rs.V3)
d.rs.V2ab <- data.frame(verb="V2",d.rs.V2ab)
d.rs.V1 <- data.frame(verb="V1",d.rs.V1)
d.rs.postV1 <- data.frame(verb="postV1",d.rs.postV1)

critdata <- rbind(d.rs.V3,d.rs.V2ab,d.rs.V1,d.rs.postV1)

critdata$times <- factor("RT")

## comparison 1:
data1 <- subset(critdata,verb=="V3")

summary(fm1 <- lmer(log(value)~ gram + (1|subj)+(1|item),
                   data=data1))

## comparison 2:

data2 <- subset(critdata,(verb=="V2" & gram=="gram") |
                                        (verb=="V1" & gram=="ungram"))

data2$wl <- nchar(as.character(data2$word))

with(data2,tapply(wl,gram,mean))
with(data2,tapply(wl,gram,se))


summary(fm2 <- lmer(log(value)~ gram +center(wl)+(1|subj)+(1|item),
                   data=data2))

## comparison 3:
data3 <- subset(critdata,(verb=="V1"))
summary(fm3 <- lmer(log(value)~ gram +(1|subj)+(1|item),
                   data=data3))

## comparison 4:
data4 <- subset(critdata,(verb=="postV1"))
summary(fm4 <- lmer(log(value)~ gram +(1|subj)+(1|item),
                   data=data4))


qq.plot(residuals(fm1))
qq.plot(residuals(fm2))
qq.plot(residuals(fm3))
qq.plot(residuals(fm4))


