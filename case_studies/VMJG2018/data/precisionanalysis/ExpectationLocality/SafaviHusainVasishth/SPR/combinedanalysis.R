datae1$expt<-factor("e1")
datae2$expt<-factor("e2")

datae1$subj<-factor(paste(datae1$expt,datae1$subj,sep=""))
datae2$subj<-factor(paste(datae2$expt,datae2$subj,sep=""))

datae1$item<-factor(paste(datae1$expt,datae1$item,sep=""))
datae2$item<-factor(paste(datae2$expt,datae2$item,sep=""))

alldata<-rbind(datae1,datae2)

m.combined<-lmer(log(rt)~pred*dist*expt+(1|subj)+(1|item),subset(alldata,rt<3000 & roi=="crit"))

summary(m.combined)

m.e2<-lmer(log(rt)~pred*dist+(1|subj)+(1|item),subset(datae2,rt<3000 & roi=="crit"))

m.e2<-lmer(log(rt)~pred+pred.dist+nopred.dist+(1|subj)+(1|item),subset(datae2,rt<3000 & roi=="crit"))

summary(m.e2)
