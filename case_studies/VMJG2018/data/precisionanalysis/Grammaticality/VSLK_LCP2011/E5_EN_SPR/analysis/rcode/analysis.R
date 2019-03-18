library(lme4)
library(car)
library(coda)
library(Hmisc)

data <- read.table("rawdata.txt")

summary(data)

colnames(data) <- c("subject","expt","item","condition","position","word","response","rt")

## question data for all data:
dataqall <-  subset(data,position=="?" & condition!="practice")
data <- subset(data,expt=="gug")

#question data for expt:
dataq <- subset(data,position=="?")

data <- subset(data,position!="?")

data$position <- as.numeric(as.character(data$position))

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
for(i in 1:n){
  if(data[i,]$item%in%c(11) & data[i,]$condition%in%c("b","d")){
    data[i,]$position <- data[i,]$position-1    
  }
}

n <- dim(data)[1]

rts.mean <- data$rt ## save the means of the two-verb regions
rts.sum <- data$rt  ## save the sum of the two-verb regions

## now for the rt averaging and summing:
for(i in 1:n){
  ## first recalculate V3 rt two ways, as an average or a sum:
  if(data[i,]$item%in%c(3,6,10,11,14) & data[i,]$position==8){
    rts.mean[i] <- (data[i,]$rt+data[i+1,]$rt)/2 ## mean
    rts.sum[i] <- data[i,]$rt+data[i+1,]$rt     ## sum
#    data[i+1,]$position <- 8
#    data[i+2,]$position <- 9
#    data[i+3,]$position <- 10
#    data[i+4,]$position <- 11
#    if(data[i,]$condition%in%c("a","b")){
#    data[i+5,]$position <- 12}
  }
  ## item 5's last verb (which consists of 2 words) needs to be averaged:
    if(data[i,]$item==5 & data[i,]$position==10 & data[i,]$condition%in%c("a","b")){
    rts.mean[i] <- (data[i,]$rt+data[i+1,]$rt)/2 ## mean
    rts.sum[i] <- data[i,]$rt+data[i+1,]$rt     ## sum
 #   data[i+1,]$position <- 10
 #   data[i+2,]$position <- 11
 #   data[i+3,]$position <- 12    
  }
    if(data[i,]$item==5 & data[i,]$position==9 & data[i,]$condition%in%c("c","d")){
    rts.mean[i] <- (data[i,]$rt+data[i+1,]$rt)/2 ## mean
    rts.sum[i] <- data[i,]$rt+data[i+1,]$rt     ## sum
#    data[i+1,]$position <- 9
#    data[i+2,]$position <- 10
#    data[i+3,]$position <- 11    
  }
}

data$rts.mean <- rts.mean
data$rts.sum <- rts.sum

### end preliminaries

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

cutoff <- 3500

ab.means <- with(subset(data.ab.crit,rt<cutoff),
                 tapply(rt,IND=list(position),mean,na.rm=TRUE))

names(ab.means) <- c("V3","V2","V1","Post-V1","Post-V1a")

ab.ci <- with(subset(data.ab.crit,rt<cutoff),
                 tapply(rt,IND=list(position),ci))

cd.means <- with(subset(data.cd.crit,rt<cutoff),
                 tapply(rt,IND=list(position),mean,na.rm=TRUE))[c(1,2,3,4)]

cd.ci <- with(subset(data.cd.crit,rt<cutoff),
                 tapply(rt,IND=list(position),ci))

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

## with lineplots:
matplot(ab.means[1:4],ylim=c(min(cdci.lower,na.rm=T),max(abci.upper,na.rm=T)),type="b",pch=1,xaxt="n",
        main="English SPR Expt. 5",cex.main=2,lwd=3,ylab="",
        cex.axis=1.8)
lines(as.vector(c(cd.means[1],NA,cd.means[2:3])),lwd=3)
points(as.vector(c(cd.means[1],NA,cd.means[2:3])),pch=19)
arrows(seq(from=1,5,by=1),abci.lower,
       seq(from=1,5,by=1),abci.upper,
       lwd =  2,col="black",angle=90,code=3,length=.1)
arrows(seq(from=1,5,by=1),cdci.lower,
       seq(from=1,5,by=1),cdci.upper,
       lwd =  2,col="black",angle=90,code=3,length=.1)
mtext(text=c("V3","V2","V1","Post-V1"),side=1,line=1.5,at=1:4,cex=2)
mtext(text="Reading time [ms]",side=2,line=2.5,cex=2)
mtext(text="Region",side=1,line=2.5,cex=2)

v1 <- rbind(subset(data.ab.crit,position=="10"),subset(data.cd.crit,position=="9"))

unique(v1$word)

v1$gram <- ifelse(v1$condition%in%c("a","b"),-1,1)
summary(modelv1 <- lmer(log(rt)~gram+(1|subject)+(1|item),v1))

postv1 <- rbind(subset(data.ab.crit,position=="11"),
                subset(data.cd.crit,position=="10"))

unique(postv1$word)

postv1$gram <- ifelse(postv1$condition%in%c("a","b"),-1,1)
summary(modelpostv1 <- lmer(log(rt)~gram+(1|subject)+(1|item),subset(postv1,rt<2500)))

qq.plot(residuals(modelpostv1))

