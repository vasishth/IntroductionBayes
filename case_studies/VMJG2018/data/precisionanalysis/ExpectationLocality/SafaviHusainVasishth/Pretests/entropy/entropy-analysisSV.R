
d<-read.table('sentcomp.txt', header=T)

#subset experiment 1 data
exp1<-subset(d, Pre.test==1)

library(plyr)
temp<-ddply(exp1,.(item.ID,cond,verb),nrow)
#head(temp)

#in three instances total count !=8
#1b     7
#3a     7
#14b    7
temp1<-ddply(temp,.(item.ID,cond),summarize,count=sum(V1))
#head(temp1)

exp1.verbcount<-merge(temp, temp1, by.x=c("item.ID", "cond"))

exp1.verbcount$prob <- exp1.verbcount$V1/exp1.verbcount$count
#head(exp1.verbcount)

exp1.verbcount[] <- lapply(exp1.verbcount, as.character)

for(i in 1:length(unique(exp1.verbcount$item.ID))) #iterate over each item
{
  cond <- unique(exp1.verbcount$cond)
  
  for(j in 1:length(cond)) #iterate over each cond
  {
    #get number of unique verbs of an item and condition
    verbs <- length(exp1.verbcount[exp1.verbcount$item.ID==i&exp1.verbcount$cond==cond[j],]$prob)
    
    sum = 0
    #compute entropy
    for(k in 1:verbs)
    {
      sum = sum + (as.numeric(exp1.verbcount[exp1.verbcount$item.ID==i&exp1.verbcount$cond==cond[j],]$prob[k])
                   *log2(as.numeric(exp1.verbcount[exp1.verbcount$item.ID==i&exp1.verbcount$cond==cond[j],]$prob[k])))
    }
    
    exp1.verbcount$entropy[exp1.verbcount$item.ID==i&exp1.verbcount$cond==cond[j]] <- -1 * sum
  }
}

#head(exp1.verbcount)

exp1.frame<-ddply(exp1.verbcount,.(item.ID,cond,count,entropy), nrow)
colnames(exp1.frame)[5] <- "unique.verbs"
#head(exp1.frame)

exp1.frame$cond<-factor(exp1.frame$cond)

#entropy
means<-with(exp1.frame,tapply(entropy,cond,mean))
stddev<-with(exp1.frame,tapply(entropy,cond,sd))
stderr<-stddev/sqrt(36)

## entropy plot for paper:
Distance<-factor(c(rep(c("short","long"),2)),levels=c("short","long"))
Predicate_Type<-factor(c(rep(c("complex","simple"),each=2)),levels=c("complex","simple"))

entropydate1<-data.frame(Predicate_Type=Predicate_Type,Distance=Distance,ent=means,se=stderr)

library(ggplot2)

entp1<-ggplot(entropydate1, aes(x=Predicate_Type, y=ent, fill=Distance)) +
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=ent-2*se, ymax=ent+2*se), 
                position=position_dodge(0.9),width=.2)+
  xlab("Predicate Type")+ylab("Entropy")+ggtitle("Intervener RC+PP (Expts 1,3)")+theme_bw()

entp1

#====================
#experiment 2

exp2<-subset(d, Pre.test==2)

temp<-ddply(exp2,.(item.ID,cond,verb),nrow)
#head(temp)

#one instance of total count !=8
#15c     6
temp1<-ddply(temp,.(item.ID,cond),summarize,count=sum(V1))
#head(temp1)

exp2.verbcount<-merge(temp, temp1, by.x=c("item.ID", "cond"))

exp2.verbcount$prob <- exp2.verbcount$V1/exp2.verbcount$count
#head(exp2.verbcount)

exp2.verbcount[] <- lapply(exp2.verbcount, as.character)

for(i in 1:length(unique(exp2.verbcount$item.ID))) #iterate over each item
{
  cond <- unique(exp2.verbcount$cond)
  
  for(j in 1:length(cond)) #iterate over each cond
  {
    #get number of unique verbs of an item and condition
    verbs <- length(exp2.verbcount[exp2.verbcount$item.ID==i&exp2.verbcount$cond==cond[j],]$prob)
    
    sum = 0
    #compute entropy
    for(k in 1:verbs)
    {
      sum = sum + (as.numeric(exp2.verbcount[exp2.verbcount$item.ID==i&exp2.verbcount$cond==cond[j],]$prob[k])
                   *log2(as.numeric(exp2.verbcount[exp2.verbcount$item.ID==i&exp2.verbcount$cond==cond[j],]$prob[k])))
    }
    
    exp2.verbcount$entropy[exp2.verbcount$item.ID==i&exp2.verbcount$cond==cond[j]] <- -1 * sum
  }
}

exp2.frame<-ddply(exp2.verbcount,.(item.ID,cond,count,entropy), nrow)
colnames(exp2.frame)[5] <- "unique.verbs"
#head(exp2.frame)

exp2.frame$cond<-factor(exp2.frame$cond)

#entropy
means<-with(exp2.frame,tapply(entropy,cond,mean))
stddev<-with(exp2.frame,tapply(entropy,cond,sd))
stderr<-stddev/sqrt(36)

Distance<-factor(c(rep(c("short","long"),2)),levels=c("short",
                                                      "long"))
Predicate_Type<-factor(c(rep(c("complex","simple"),each=2)),levels=c("complex","simple"))

entropydate2<-data.frame(Predicate_Type=Predicate_Type,Distance=Distance,ent=means,se=stderr)

entp2<-ggplot(entropydate2, aes(x=Predicate_Type, y=ent, fill=Distance)) +
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=ent-2*se, ymax=ent+2*se), 
                position=position_dodge(0.9),width=.2)+
  xlab("Predicate Type")+ylab("Entropy")+ggtitle("Intervener PP (Expts 2, 4)")+scale_y_continuous(limits = c(0, 4))+theme_bw()

#entp2

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(entp1,entp2,cols=2)


#high predictibility conditions long vs short difference in entropy (Exp1 vs Exp2)
diffe1ba<-subset(exp1.frame,cond=="b")$entropy-subset(exp1.frame,cond=="a")$entropy
diffe2ba<-subset(exp2.frame,cond=="b")$entropy-subset(exp2.frame,cond=="a")$entropy
t.test(diffe1ba,diffe2ba,paired=T)

#head(exp1.frame)
exp1entropy<-exp1.frame[,c(1,2,4)]
exp2entropy<-exp2.frame[,c(1,2,4)]

#SPR exp1 data
data1<-read.table("../../data_spr/Persiane1.txt", header=T)
data1$pos<-factor(data1$pos)
data1$resp<-factor(data1$resp)
data1$cond<-factor(data1$cond)
## short -1, long 1
data1$dist<-ifelse(data1$cond%in%c("a","c"),-1,1)
## pred 1, -1
data1$pred<-ifelse(data1$cond%in%c("a","b"),1,-1)

#nested
data1$pred.dist <- ifelse(data1$cond=="a",-1,
                          ifelse(data1$cond=="b",1,0))
data1$nopred.dist <- ifelse(data1$cond=="c",-1,
                            ifelse(data1$cond=="d",1,0))

## merge with data:
data1crit<-subset(data1,roi=="crit" & rt<3000)

data1critent<-merge(data1crit,exp1entropy,by.x=c("item","cond"),
                    by.y=c("item.ID","cond"))

## sanity check
#dim(data1crit)
#dim(data1critent)
#summary(data1critent)

## center:
data1critent$c_ent<-scale(data1critent$entropy,scale=F)

library(lme4)

m1ent<-lmer(log(rt)~pred*dist*c_ent+(1+dist+pred:dist||subj)+(1+c_ent||item),data1critent)
summary(m1ent)


#sentence completion target/condition & grammaticality/condition analysis

exp1abo<-read.table("exp1aborevised.txt",header=TRUE)

library(rstanarm)

#grammaticality (experiment 1 a vs b)

#grammaticality in the long vs short condition in experiment 1
with(exp1abo,tapply(gram,cond,mean))

mstangram<-stan_glmer(gram~condition+(1+condition|subject)+(condition|item.ID),
                      family=binomial(),
                      prior_intercept=student_t(df=2),
                      prior=student_t(df=2),
                      prior_covariance=decov(regularization=2),
                      exp1abo,
                      chains=4,
                      iter=2000,
                      cores=4)

## summary for paper: 

#[Note: numbers do not match with the ones in the paper]
#[From the paper]
#The results of the model fit showed a weak reduction in grammaticality of sentence completions
#in the long vs short conditions; the log odds were −0.29[−0.73, 0.09], with a probability of the log odds
#being negative being 0.93.

posterior_interval(mstangram, prob = 0.95, pars = "condition")

samples_m1new <- as.data.frame(mstangram) 
posterior_condition <- samples_m1new$condition
mean(posterior_condition) 
mean(posterior_condition<0)

## no reduction in grammaticality of target in experiment 2:
exp2abo<-read.table("expt2ab.txt",header=TRUE)
with(exp2abo,tapply(gram,cond,mean))


## load sentence completion data:
d<-read.table('sentcomp.txt', header=T)

## Expt 1 analysis
exp1<-subset(d, Pre.test==1)
## Isolate a,b conditions:
exp1ab<-subset(exp1,cond%in%c("a","b"))
exp1ab$cond<-factor(exp1ab$cond)
#summary(exp1ab)

## target verbs in sentence completion study,
## these are the verbs subjects should produce:
target_verbs<-c("kardan","zadan",
                "kardan","kardan",
                "kardan","kardan",
                "kardan","dashtan",
                "bordan","kardan",
                "zadan","zadan",
                "zadan","zadan",
                "zadan","zadan",
                "kardan","kardan",
                "kardan","kardan",
                "dadan","kardan",
                "dadan","dadan",
                "goftan","kardan",
                "kardan","kardan",
                "gozashtan","zadan",
                "kardan","kardan",
                "kardan","kardan",
                "gereftan","kardan")

targets<-data.frame(item=rep(1:36,each=2),
                    cond=rep(letters[1:2],36),
                    target_verbs=rep(target_verbs,each=2))

nrows<-dim(exp1ab)[1]

## Next, create a column marking "correct completion"
hit<-rep(NA,nrows)

## identify exact matches with target:
for(i in 1:nrows){
  ## get i-th row:
  tmp<-exp1ab[i,]
  itemid<-tmp$item.ID
  condition<-as.character(tmp$cond)
  tmpverb<-as.character(tmp$verb)
  ## find appropriate row in targets data frame:
  target_row<-subset(targets,item==itemid & 
                       cond==condition)
  targetverb<-as.character(target_row$target_verbs)
  if(targetverb==tmpverb){
    hit[i]<-1} else {hit[i]<-0}
}

## 1 if target verb produced, 0 otherwise
exp1ab$target<-hit

exp1ab$condition<-ifelse(exp1ab$cond=="b",1,-1)
exp1ab$subj.ID<-factor(exp1ab$subj.ID)

## Fix problem by pasting list no. to make subjects unique:
exp1ab$subject<-factor(paste(exp1ab$subj.ID,exp1ab$LatSq.list,sep=""))

#target completion (experiment 1, a v b)
mstan<-stan_glmer(target~condition+(1+condition|subject)+(condition|item.ID),
                  family=binomial(),
                  prior_intercept=student_t(df=2),
                  prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  exp1ab,
                  chains=4,
                  iter=2000,
                  cores=4)

## summary for paper:

#[Note: numbers slighly different. This is probably expected??]

#[From the paper]
#On the log-odds scale, the mean
#and 95% uncertainty interval were −0.29, [−0.78, 0.17], and the posterior probability of the reduction
#being less than 0 was 0.89. The odds ratio of producing a target verb in the long vs short condition was
#0.75, with 95% uncertainty interval [0.46, 1.2]

ci95<-posterior_interval(mstan, prob = 0.95, pars = "condition")
ci95
samples_m1 <- as.data.frame(mstan) 
#str(samples_m1)
posterior_condition_target <- samples_m1$condition
options(digits = 4) 
mean(posterior_condition_target) 

## prob less than 0
mean(posterior_condition_target<0)

## odds ratio:
exp(-0.29)

## lower 
exp(ci95[1])
## upper
exp(ci95[2])


#target completion analysis experiment 2
mstan.target.expt2<-stan_glmer(target~condition+(1+condition|subj.ID)+(condition|item.ID),
                  family=binomial(),
                  prior_intercept=student_t(df=2),
                  prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  exp2abo,
                  chains=4,
                  iter=2000,
                  cores=4)

## summary for paper:

#[Note: numbers slighly different. This is probably expected??]

#[From the paper]
#the probability of producing the target verb fell: −0.21[−0.5, 0.1]; the posterior probability of
#the reduction being less than 0 was 0.91.
#The odds ratio of producing a target verb in the long vs short condition was 0.81, 
#with 95% uncertainty intervals [0.58, 1.11]

posterior_interval(mstan.target.expt2, prob = 0.95, pars = "condition")

samples_m2 <- as.data.frame(mstan.target.expt2)
posterior_condition_target_expt2 <- samples_m2$condition
options(digits = 4) 
mean(posterior_condition_target_expt2) 

## prob less than 0
mean(posterior_condition_target_expt2<0)

## odds ratio:
exp(-0.16)
## lower 
exp(ci95[1])
## upper
exp(ci95[2])


## plot posterior (target completion, experiment 1/2, a vs b):
postcond1<-data.frame(post=posterior_condition_target)

posthist1<-ggplot(postcond1,aes(x=post))+
  geom_histogram(aes(y=..density..),
                 binwidth=.1,
                 colour="black", fill="white") +
  xlab("posterior")+ggtitle("Expt 1")+
  annotate("segment", x = ci95[1], 
           xend = ci95[2], 
           y = .25, yend = .25,
           colour = "red",size=2)+
  theme_bw()

posthist1

#different from the plot in the paper!
postcond2<-data.frame(post=posterior_condition_target_expt2)

posthist2<-ggplot(postcond2,aes(x=post))+
  geom_histogram(aes(y=..density..),
                 binwidth=.1,
                 colour="black", fill="white") +
  xlab("posterior")+ggtitle("Expt 1")+
  annotate("segment", x = ci95[1], 
           xend = ci95[2], 
           y = .25, yend = .25,
           colour = "red",size=2)+
  theme_bw()

posthist2

#synonym analysis

#proportion of N-V completions
pat<-grepl('-', subset(exp1abo, cond=='a')$verb)
length(pat[pat==TRUE])/length(pat)

pat<-grepl('-', subset(exp1abo, cond=='b')$verb)
length(pat[pat==TRUE])/length(pat)

pat<-grepl('-', subset(exp2abo, cond=='a')$verb)
length(pat[pat==TRUE])/length(pat)

pat<-grepl('-', subset(exp2abo, cond=='b')$verb)
length(pat[pat==TRUE])/length(pat)


## targets produced even if they were not 
## exact matches, regardless of ungrammaticality:
exp1abo$target2<-ifelse(exp1abo$target==0 & 
                          exp1abo$synonym==1,1,
                        ifelse(exp1abo$target==1,1,0))

with(exp1abo,tapply(synonym,cond,sum))

mstannew<-stan_glmer(target2~condition+
                       (1+condition|subject)+
                       (condition|item.ID),
                     family=binomial(),
                     prior_intercept=student_t(df=2),
                     prior=student_t(df=2),
                     prior_covariance=decov(regularization=2),
                     exp1abo,
                     chains=4,
                     iter=2000,
                     cores=4)


#[Note: numbers slighly different. This is probably expected!!]
#In experiment 1, the log odds are −0.21[−0.71, 0.23], with the probability of the log odds being less
#than 0 being 0.82.

posterior_interval(mstannew, prob = 0.95, pars = "condition")

samples_m1new <- as.data.frame(mstannew) 
posterior_condition <- samples_m1new$condition
options(digits = 4) 
mean(posterior_condition) 

mean(posterior_condition<0) 


#experiment 2
exp2abo$target2<-ifelse(exp2abo$target==0 & 
                          exp2abo$synonym==1,1,
                        ifelse(exp2abo$target==1,1,0))


mstannew2<-stan_glmer(target2~condition+
                       (1+condition|subj.ID)+
                       (condition|item.ID),
                     family=binomial(),
                     prior_intercept=student_t(df=2),
                     prior=student_t(df=2),
                     prior_covariance=decov(regularization=2),
                     exp2abo,
                     chains=4,
                     iter=2000,
                     cores=4)

posterior_interval(mstannew2, prob = 0.95, pars = "condition")


#[Note: numbers slighly different. This is probably expected!!]
#In experiment 2, the log odds are −0.29[−0.73, 0.09], with the probability of log odds
#being less than 0 being 0.93.

samples_m2new <- as.data.frame(mstannew2) 
posterior_condition <- samples_m2new$condition
options(digits = 4) 
mean(posterior_condition) 

mean(posterior_condition<0) 


