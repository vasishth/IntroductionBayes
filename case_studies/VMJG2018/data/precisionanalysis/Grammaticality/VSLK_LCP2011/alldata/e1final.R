e1<-read.table("e1critdata.txt",header=T)



contrasts(e1$gram)

## at region V1, gram slower than ungram. 
## H0: gram=ungram

## t-test paired

## lm

V1<-subset(e1,region=="V1")

summary(lm(value~gram,V1))

## one submitted solution:
e1<-read.table("e1critdata.txt")

head(e1)

#subset for V1
V1 <-subset(e1,region=="V1")

xtabs(~subject+gram,V1)

#subject 4 has no value for cond b which also leads to missing data for gram -> we need to get rid of it
data<-subset(V1,subject!="4")

data<-V1

#mean for subjects
data2<- aggregate(data$value, list(data$subject,data$gram),mean)

#check
head(data2)

#renaming columns
colnames(data2)<-c("subject","condition","RT")

#subsets for gramm and ungramm
gramm<-subset(data2, condition=="gram")
ungramm <-subset(data2, condition=="ungram")

mean(gramm$RT)

mean(ungramm$RT)

mean(ungramm$RT)-mean(gramm$RT)

#t-test (paired)
t.test(ungramm$RT, gramm$RT, paired=T)

#t-test (unpaired)
t.test(ungramm$RT, gramm$RT, paired=F)

## linear model
m0<-lm(RT~condition,data2)

summary(m0)

library(lme4)

## linear mixed model
(m1<-lmer(RT~condition+(1|subject),data2))

## unaggregated data:
(m2<-lmer(value~gram+(1|subject)+(1|item),data))



 

