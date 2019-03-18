e2<-read.table("e2critdata.txt",header=T)

V3<-subset(e2,region=="V3")

means<-with(V3,tapply(value,condition,mean))
G.mean<-mean(means)


xtabs(~subject+gram,V3)

## conditions:
#   Gram     Interference
#a: gram     hi
#b: gram     hi
#c: ungram   lo
#d: ungram   lo


## compare a with b, a with c, a with d:
cmat.1<-contr.treatment(4)
rownames(cmat.1)<-letters[1:4]
colnames(cmat.1)<-letters[2:4]

contrasts(V3$condition)<-cmat.1

library(lme4)

(m0<-lmer(value~condition+(1|subject),V3))

summary(lm(value~condition,V3))
## treatment contrast:
means[2]-means[1]
## etc.

## computing p-values:
library(languageR)
m0.mcmc<-pvals.fnc(m0)
m0.mcmc$fixed

## sum contrast:
# the first three means compared with the grand mean
cmat.2<-contr.sum(4)
rownames(cmat.2)<-letters[1:4]
colnames(cmat.2)<-letters[2:4]

contrasts(V3$condition)<-cmat.2

(m1<-lmer(value~condition+(1|subject),V3))


## compare a with b, b with c, c with d:
library(MASS)
cmat.3<-contr.sdif(4)
rownames(cmat.3)<-letters[1:4]
colnames(cmat.3)<-c(".2-1",".3-2",".4-3")
contrasts(V3$condition)<-cmat.3

(m2<-lmer(value~condition+(1|subject),V3))


## helmert contrasts:
# average of a,b,c with d
# average of a,b with c
# a with b
cmat.4<-contr.helmert(4)
rownames(cmat.4)<-letters[1:4]
colnames(cmat.4)<-c(".abc",".ab",".a")
contrasts(V3$condition)<-cmat.4
(m3<-lmer(value~condition+(1|subject),V3))

## anova style comparisons:
## main effects of gram and int
## interaction
cmat.5<- matrix(c(-1/2, -1/2, +1/2, +1/2, 
                 -1/2, +1/2, -1/2, +1/2, 
                  1/2, -1/2, -1/2, +1/2), 
                  4, 3, 
                  dimnames = list(c("a",
"b", "c", "d"), c(".gram", ".int", ".interaction")))

contrasts(V3$condition)<-cmat.5
(m4<-lmer(value~condition+(1|subject),V3))








