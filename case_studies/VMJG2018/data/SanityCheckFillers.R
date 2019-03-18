# Vasishth et al.: The statistical significance filter leads to overoptimistic expectations of replicability
# Sanity check on filler items: Is there a frequency effect as expected?

setwd("~/Documents/Projects/StatisticalSignificanceFilter/StatisticalSignificanceFilter/VMJG2018/data")
library(lme4)

# Eyetracking data
e2 <- read.table('Exp2fillersFreq.txt', header=TRUE)
summary(e2)

e4 <- read.table('Exp4fillersFreq.txt', header=TRUE)
e4$subject <- e4$subject+100
summary(e4)

e6 <- read.table('Exp6fillersFreq.txt', header=TRUE)
e6$subject <- e6$subject+200
summary(e6)

e7 <- read.table('Exp7fillersFreq.txt', header=TRUE)
e7$subject <- e7$subject+300
summary(e7)


d <- rbind(e2,e4,e6, e7)
d$subject <- factor(d$subject)
length(unique(d$subject))
d$itemid <- factor(d$itemid)
d$FPR<-as.logical(d$RPD-d$FPRT)



# length and log-Freq are strongly negatively correlated:
cor(d$type_logFreq, d$len)


# z-score on all dat
d$type_logFreq_z <- (d$type_logFreq-mean(d$type_logFreq))/sd(d$type_logFreq)
d$len_z <- (d$len-mean(d$len))/sd(d$len)

# TODO decorrelate length and frequency



fprt <-  subset(d, FPRT>0)
mfprt <- lmer(log(FPRT) ~ len_z + type_logFreq_z  + (1+len_z + type_logFreq_z|subject)+(1+len_z + type_logFreq_z|itemid), data = fprt) 
summary(mfprt)


