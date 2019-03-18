e2<-read.table("e2critdata.txt",header=T)

V3<-subset(e2,region=="V3")

xtabs(~subject+gram,V3)

## conditions:
#   Gram     Interference
#a: gram     hi
#b: gram     hi
#c: ungram   lo
#d: ungram   lo

## treatment contrast:
## compare a with b, a with c, a with d:
cmat.1<-contr.treatment(4)
rownames(cmat.1)<-letters[1:4]
colnames(cmat.1)<-letters[2:4]

contrasts(V3$condition)<-cmat.1

library(lme4)

m0<-lmer(value~condition+(1|subject),V3)

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
colnames(cmat.3)<-letters[2:4]


## contrast coding:



#V3$subject<-factor(V3$subject)

library(lme4)

(m.pooled<-lm(value~1,V3))
(m.unpooled<-lm(value~factor(subject)-1,V3))
(m.lmer<-lmer(value~1+(1|subject),V3))

pooled.coef<-coef(m.pooled)

unpooled.coefs.se<-as.data.frame(summary(m.unpooled)[4])[,c(1,2)]

colnames(unpooled.coefs.se)<-c("coef","se")

counts<-xtabs(~subject+gram,V3)
countssum<-rowSums(counts)

unpooled.coefs.se$countssum<-countssum

unpooled.coefs.se.ordered<-unpooled.coefs.se[order(unpooled.coefs.se$countssum),]

plot(unpooled.coefs.se.ordered$countssum,
     unpooled.coefs.se.ordered$se,
     main="SE estimates for subjects by sample size",ylab="se",xlab="sample size")

plot(unpooled.coefs.se.ordered$countssum,
     unpooled.coefs.se.ordered$coef,
     ylab="unpooled coefficients",xlab="sample size",main="Unpooled coefficients for subjects by sample size")
abline(coef(m.pooled),0)

lmer.ranef<-ranef(m.lmer)

unpooled.coefs.se$ranef<-as.vector(lmer.ranef)

(partialpooling<-lmer(value~gram+(1|subject),V3))

print(dotplot(ranef(partialpooling, post = TRUE),
              strip=FALSE,main="Predictions for Participants Random Effects")$subject)

fixef(partialpooling)
ranef(partialpooling)

contrasts(V3$gram)

fixef(m0)

means<-aggregate(V3$value,list(V3$subject,V3$gram),mean)

colnames(means)<-c("subject","cond","RRT")

gram<-subset(means,cond=="gram")

ungram<-subset(means,cond=="ungram")

t.test(ungram$RRT,gram$RRT,paired=T)

g.mean<-mean(gram$RRT)
u.mean<-mean(ungram$RRT)

d.g<-gram$RRT-g.mean
gram$d.g<-d.g

d.u<-ungram$RRT-u.mean
ungram$d.u<-d.u

gram$datapoints<-counts[1:47,2]

plot(gram$datapoints,gram$d.g)
abline(0,0)


d.u<-ungram$RRT-u.mean

counts<-xtabs(~subject+gram,V3)

eval<-c(3,2,5)
beauty<-c(0,-1,2)

m0<-lm(eval~beauty)

residuals(m0)











