library(lme4)
library(car)
library(Hmisc)
library(xtable)

data <- read.table("../../data/rawdata.txt")

summary(data)

colnames(data) <- c("subject","expt","item","condition","position","word","response","rt")

## question data for all data:
dataqall <-  subset(data,position=="?" & condition!="practice")

## mean correct responses overall:
mean(with(dataqall,tapply(as.numeric(as.character(response)),subject,mean)))

data <- subset(data,expt=="gug")

#question data for expt:
dataq <- subset(data,position=="?")

dataq$condition <- factor(dataq$condition)
dataq$response <- as.numeric(as.character(dataq$response))

## code up grammaticality factor:
dataq$gram <- ifelse(dataq$condition%in%c("a","b"),1,-1)
## code up interference factor:
dataq$int <- ifelse(dataq$condition%in%c("a","c"),1,-1)

## means and se of engug expt responses by condition:
with(dataq,tapply(response,gram,mean,na.rm=TRUE))*100
with(dataq,tapply(response,gram,se))*100

with(dataq,tapply(response,int,mean,na.rm=TRUE))*100
with(dataq,tapply(response,int,se))*100

summary(lmer(response~gram*int+(1|subject)+(1|item),
             family=binomial(),dataq))

data <- subset(data,position!="?")

data$position <- as.numeric(as.character(data$position))

data$condition <- factor(data$condition)

## first verb:
## average item 3 position 8 and 9
##           V3  V2 V1 Post-V1 Post-V1a
## original: 8,9 10 11 12      13
## recode:   8   9  10 11      12
## average item 6 position 8 and 9 
## average item 10 position 8 and 9
## average item 14 position 8 and 9
## average item 5 (original, not recoded) position 11 and 12
## average item 11ac position 8 and 9 
## average item 11bd recode position 9 and 10 to 8 and 9 and average 

##first recode the positions in item 11b,d by just
## reducing the position index by 1; note that now position 0 is -1.
## item 11 has a two-word NP2, so everything is pushed forward by 1.
n <- dim(data)[1]
for(i in 1:n){
  if(data[i,]$item%in%c(11) & data[i,]$condition%in%c("b","d")){
    data[i,]$position <- data[i,]$position-1    
  }
}

data.ab <- subset(data,condition%in%c("a","b"))
data.cd <- subset(data,condition%in%c("c","d"))

data.ab.crit <- subset(data.ab,position%in%8:12)
#data.ab.crit$position <- factor(data.ab.crit$position)

unique(subset(data.ab.crit,position==9)$word)
## item 3
subset(data.ab.crit,position==9 & word=="for,")
#The worker, who the tenant, that the foreman looked for, injured, questioned the shepherd.
#The worker, who the bucket, that the foreman looked for, injured, questioned the shepherd.
## item 6
subset(data.ab.crit,position==9 & word=="along,")
## item 11
subset(data.ab.crit,position==9 & word=="about,")
## items 10 and 14
subset(data.ab.crit,position==9 & word=="at,")

for(i in 1:dim(data.ab.crit)[1]){
  if(data.ab.crit[i,]$position==9 & data.ab.crit[i,]$item%in%c(3,6,10,11,14)){
    data.ab.crit[i,]$position <- 8
  }
}
unique(subset(data.ab.crit,position==8)$word)

unique(subset(data.ab.crit,position==9)$word)
for(i in 1:dim(data.ab.crit)[1]){
  if(data.ab.crit[i,]$position==10 & data.ab.crit[i,]$item%in%c(3,6,10,11,14)){
    data.ab.crit[i,]$position <- 9
  }
}
unique(subset(data.ab.crit,position==9)$word)

unique(subset(data.ab.crit,position==10)$word)

for(i in 1:dim(data.ab.crit)[1]){
  if(data.ab.crit[i,]$position==11 & data.ab.crit[i,]$item%in%c(3,6,10,11,14)){
    data.ab.crit[i,]$position <- 10
  }
}
unique(subset(data.ab.crit,position==10)$word)

unique(subset(data.ab.crit,position==11)$word)

for(i in 1:dim(data.ab.crit)[1]){
  if(data.ab.crit[i,]$position==11 & data.ab.crit[i,]$item==5){
    data.ab.crit[i,]$position <- 10
  }
}
unique(subset(data.ab.crit,position==11)$word)


data.cd.crit <- subset(data.cd,position%in%8:11)
#data.cd.crit$position <- factor(data.cd.crit$position)

unique(subset(data.cd.crit,position==9)$word)
## item 3
subset(data.cd.crit,position==9 & word=="for,")
#The worker, who the tenant, that the foreman looked for, injured, questioned the shepherd.
#The worker, who the bucket, that the foreman looked for, injured, questioned the shepherd.
## item 6
subset(data.cd.crit,position==9 & word=="along,")
## item 11
subset(data.cd.crit,position==9 & word=="about,")
## items 10 and 14
subset(data.cd.crit,position==9 & word=="at,")

for(i in 1:dim(data.cd.crit)[1]){
  if(data.cd.crit[i,]$position==9 & data.cd.crit[i,]$item%in%c(3,6,10,11,14)){
    data.cd.crit[i,]$position <- 8
  }
}
unique(subset(data.cd.crit,position==8)$word)

for(i in 1:dim(data.cd.crit)[1]){
  if(data.cd.crit[i,]$position==10 & data.cd.crit[i,]$item%in%c(3,6,10,11,14)){
    data.cd.crit[i,]$position <- 9
  }
}
unique(subset(data.cd.crit,position==9)$word)

for(i in 1:dim(data.cd.crit)[1]){
  if(data.cd.crit[i,]$position==11 & data.cd.crit[i,]$item%in%c(3,6,10,11,14)){
    data.cd.crit[i,]$position <- 10
  }
}
unique(subset(data.cd.crit,position==10)$word)

for(i in 1:dim(data.cd.crit)[1]){
  if(data.cd.crit[i,]$position==10 & data.cd.crit[i,]$item==5){
    data.cd.crit[i,]$position <- 9
  }
}

unique(subset(data.cd.crit,position==10)$word)

## end preliminaries

v1 <- rbind(subset(data.ab.crit,position=="10"),subset(data.cd.crit,position=="9"))
summary(v1)

postv1 <- rbind(subset(data.ab.crit,position=="11"),
                subset(data.cd.crit,position=="10"))

v1 <- data.frame(v1,roi="v1")
postv1 <- data.frame(postv1,roi="postv1")

crit.data <- rbind(v1,postv1)

write(t(crit.data),ncolumns=9,file="VSLKcritdataMiSPR.txt")

cutoff <- 2000
ab.means <- with(subset(data.ab.crit,rt<cutoff),
                 tapply(rt,IND=list(position),mean,na.rm=TRUE))

#ab.means <- with(data.ab.crit,
#                 tapply(rt,IND=list(position),mean,na.rm=TRUE))

names(ab.means) <- c("V3","V2","V1","Post-V1","Post-V1a")

ab.ci <- with(subset(data.ab.crit,rt<cutoff),
                 tapply(rt,IND=list(position),ci))

#ab.ci <- with(data.ab.crit,
#                 tapply(rt,IND=list(position),ci))

#ab.ci <- with(subset(data.ab.crit,rt<cutoff),
#                 tapply(rt,IND=list(position),ci))


cd.means <- with(subset(data.cd.crit,rt<cutoff),
                 tapply(rt,IND=list(position),mean,na.rm=TRUE))[c(1,2,3,4)]

#cd.means <- with(data.cd.crit,
#                 tapply(rt,IND=list(position),mean,na.rm=TRUE))[c(1,2,3,4)]


cd.ci <- with(subset(data.cd.crit,rt<cutoff),
                 tapply(rt,IND=list(position),ci))

#cd.ci <- with(data.cd.crit,
#                 tapply(rt,IND=list(position),ci))

abci.lower <- c(ab.ci$`8`$lower,ab.ci$`9`$lower,
          ab.ci$`10`$lower,ab.ci$`11`$lower,
          ab.ci$`12`$lower)

abci.upper <- c(ab.ci$`8`$upper,ab.ci$`9`$upper,
                ab.ci$`10`$upper,ab.ci$`11`$upper,
                ab.ci$`12`$upper)

cdci.lower <- c(cd.ci$`8`$lower,NA,cd.ci$`9`$lower,
          cd.ci$`10`$lower,cd.ci$`11`$lower)

cdci.upper <- c(cd.ci$`8`$upper,NA,cd.ci$`9`$upper,
                cd.ci$`10`$upper,cd.ci$`11`$upper)

createPS("engugexpt5.ps")
## with lineplots:

matplot(ab.means[1:4],ylim=c(min(cdci.lower,na.rm=T),max(abci.upper,na.rm=T)),
        type="b",pch=16,xaxt="n",
        main="Experiment 5 (English SPR)",cex.main=2,lwd=3,ylab="",
        cex.axis=1.8)

lines(as.vector(c(cd.means[1],NA,cd.means[2:3])),lwd=3,col=gray(.5),lty=2)
points(as.vector(c(cd.means[1],NA,cd.means[2:3])),pch=19,col=gray(.5))

arrows(seq(from=1,5,by=1),abci.lower,
       seq(from=1,5,by=1),abci.upper,
       lwd =  2,col="black",angle=90,code=3,length=.1)

arrows(seq(from=1,5,by=1),cdci.lower,
       seq(from=1,5,by=1),cdci.upper,
       lwd =  2,col=gray(0.5),angle=90,code=3,length=.1)

mtext(text=c("V3","V2","V1","Post-V1"),side=1,line=1.5,at=1:4,cex=2)
mtext(text="Reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)


dev.off()


unique(v1$word)

## old:
#      g  g  u  u
#      h  l  h  l
##     a  b  c  d
#gram -1 -1  0  0 
#int  -1  1 -1  1 
#gxi  -1  1  1 -1

## new:
#      g  g  u  u
#      h  l  h  l
##     a  b  c  d
#gram  1  1 -1 -1 
#int   1 -1  1 -1 
#gxi   1 -1 -1  1



v1$gram <- ifelse(v1$condition%in%c("a","b"),1,-1)
v1$int <- ifelse(v1$condition%in%c("a","c"),1,-1)
v1$gxi <- ifelse(v1$condition%in%c("a","d"),1,-1)

summary(modelv1 <- lmer(log(rt)~gram+int+gxi+(1|subject)+(1|item),subset(v1,rt<2000)))

qq.plot(residuals(modelv1))

postv1 <- rbind(subset(data.ab.crit,position=="11"),
                subset(data.cd.crit,position=="10"))

unique(postv1$word)

postv1$gram <- ifelse(postv1$condition%in%c("a","b"),1,-1)
postv1$int <- ifelse(postv1$condition%in%c("a","c"),1,-1)
postv1$gxi <- ifelse(postv1$condition%in%c("a","d"),1,-1)

summary(modelpostv1 <- lmer(log(rt)~gram+int+gxi+(1|subject)+(1|item),
                            subset(postv1,rt<2000)))

summary(modelpostv1 <- lmer(log(rt)~gram+int+gxi+(1|subject)+(1|item),
                            postv1))

qq.plot(residuals(modelpostv1))

mc.v1     <- mcmcsamp(modelv1,50000)
mc.postv1 <- mcmcsamp(modelpostv1,50000)

hpd.modelv1 <- lmerHPD(mc.v1)
hpd.modelpostv1 <- lmerHPD(mc.postv1)
