setwd("C:/Users/Farnoosh/Desktop")
exp2ab<-read.table("expt2ab.txt", header=T)
head(exp2ab)

target_verbs<-c("kardan","zadan",
                "kardan","kardan",
                "kardan","kardan",
                "kardan","dashtan",
                "bordan","kardan",
                "zadan","zadan",
                "zadan","zadan",
                "zadan","zadan",
                "kardan","kardan",
                "dadan","kardan",
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

nrows<-dim(exp2ab)[1]

## column marking "correct completion"
hit<-rep(NA,nrows)

## identify exact matches with target:
for(i in 1:nrows){
  ## get i-th row:
  tmp<-exp2ab[i,]
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
exp2ab$mytarget<-hit

## slight reduction in prob. of target:
with(exp2ab,tapply(target,cond,mean))

with(exp2ab,tapply(mytarget,cond,mean))

## my for loop based targets don't match Farnoosh's:
mismatch<-which(exp2ab$target!=exp2ab$mytarget)
exp2ab[mismatch,] ## mine are correct.

## overwrite the old target column, which has mistakes:
exp2ab$target<-hit

##############################check if everything is OK :
summary(exp2ab)
colnames(exp2ab)

## number of subjects : 32
unique(exp2ab$subj.ID)

## number of items :36
unique(exp2ab$item.ID)

## number of conds: 2 (a and b), condition: -1,1
unique(exp2ab$cond)
unique(exp2ab$condition)

## number of subjects and items per condition
xtabs(~item.ID+cond,exp2ab)
xtabs(~subj.ID+cond,exp2ab)

## number of synonyms in relation to target (50 cases where it was a synonym, and not a target)
xtabs(~target+synonym,exp2ab)

## number of ungrammatical items in short versus long conds. :
xtabs(~gram+cond,exp2ab)

#################################

exp2ab$subj.ID<-factor(exp2ab$subj.ID)



## Fit model to check if there is a 
## significant reduction in prob. of 
## completing the sentence with correct verb
## in long vs short distance conditions:
library(lme4)
mtarget2<-glmer(target~condition+
                 (1+condition||subj.ID)+
                 (1+condition||item.ID),
               family=binomial(),
               exp2ab)
summary(mtarget2)

length(exp2ab$item.ID)
length(unique(exp2ab$item.ID))

## grammaticality:
with(exp2ab,tapply(gram,IND=list(cond),mean))

## Plots 
barplot(with(exp2ab,tapply(gram,IND=list(cond,item.ID),mean)),beside=TRUE)

barplot(with(exp2ab,tapply(gram,IND=list(cond,subj.ID),mean)),beside=TRUE)

## treating non-targets that are synonymous as targets:
 
## if target 0 and synonym 1, then create
## new column called target2 and use that for
## analysis

## targets produced even if they were not 
## exact matches, regardless of ungrammaticality:
exp2ab$target2<-ifelse(exp2ab$target==0 & 
                          exp2ab$synonym==1,1,
                        ifelse(exp2ab$target==1,1,0))

## relaxed version:
with(exp2ab,tapply(target2,cond,mean))
## cf. strict version:
with(exp2ab,tapply(target,cond,mean))

## grammaticality:
with(exp2ab,tapply(gram,IND=list(cond),mean))
barplot(with(exp2ab,tapply(gram,IND=list(cond,item.ID),mean)),beside=TRUE)

barplot(with(exp2ab,tapply(gram,IND=list(cond,subj.ID),mean)),beside=TRUE)

xtabs(~gram+cond,exp2ab)

mgram<-glmer(gram~condition+
               (1|item.ID),
             family=binomial(),
             exp2ab)

summary(mgram)

##### Reproduced upto here.
##### Note to Shravan

###### I couldn't reproduce the results of the following Bayesian analysis (as I couldn't install the packages "rstanarm" and "SPIn)
###### However, they can be reproduced in another system with these packages as other minor modifications like
###### changing the columns have been made. I need to study more about this method. 



##################################


## Bayesian analysis of the same:
## want to fit a maximal model here
## for maximum conservativity:

## Priors: plausible values of 
## prob range from 0.0001 to 0.9999
## Intercept: t(2) a bit like N(0,2.96^2)
## Slope: t(2) a bit like N(0,2.96^2)

library(rstanarm)
mstan<-stan_glmer(target~condition+(1+condition|subj.ID)+(condition|item.ID),
                  family=binomial(),
                  prior_intercept=student_t(df=2),
                  prior=student_t(df=2),
                  prior_covariance=decov(regularization=2),
                  exp2ab,
                  chains=4,
                  iter=2000,
                  cores=4)

## Summarize results:
## extract random effects by item,
## which are very variable:
asmatsummary<-as.matrix(summary(mstan))[39:74,]

## get mean, and 95% bounds for each item:
asmatsummary<-asmatsummary[,c(1,4,8)]

ranefsummary<-as.data.frame(asmatsummary)
colnames(ranefsummary)<-c("mean","lower","upper")
rownames(ranefsummary)<-1:36

ranefsummary$item<-factor(1:36)

ranefsummary<-ranefsummary[with(ranefsummary, order(mean)), ]

p <- ggplot(ranefsummary, aes(mean, item))
p + geom_point() +
  geom_errorbarh(aes(xmax = lower, xmin = upper))+
  xlab("item level adjustment to slope")+
  geom_vline(xintercept=0)+
  theme_bw()

## summary for paper:
ci95 <- posterior_interval(mstan, prob = 0.95, pars = "condition")

samples_m1 <- as.data.frame(mstan) 
#str(samples_m1)
posterior_condition <- samples_m1$condition
options(digits = 4) 
mean(posterior_condition) 

## prob less than 0
mean(posterior_condition<0)

## plot posterior:
postcond<-data.frame(post=posterior_condition)

posthiste2<-ggplot(postcond,aes(x=post))+
  geom_histogram(aes(y=..density..),
                 binwidth=.1,
                 colour="black", fill="white") +
  xlab("posterior")+ggtitle("Expt 2")+
  annotate("segment", x = ci95[1], 
           xend = ci95[2], 
           y = .25, yend = .25,
           colour = "red",size=2)+theme_bw()

posthiste2

## plot both histograms side by side:
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

## posthist comes from entropySV.R
multiplot(posthist,posthiste2,cols=1)

library(SPIn) # For calculating the HPDI
bootSPIn(posterior_condition)$spin # 95% HPDI  

## odds ratio:
exp(-0.21)

## lower 
exp(ci95[1])
## upper
exp(ci95[2])

###############################

## now we do a second analysis, treating
## non-targets that are synonymous as targets:
exp2abo<-read.table("expt2ab.txt",header=TRUE)
dim(exp2abo)
summary(exp2abo)

head(exp2abo)

xtabs(~target+synonym,exp2abo)

summary(subset(exp2abo,target==1))

## if target 0 and synonym 1, then create
## new column called target2 and use that for
## analysis
exp2abo<-exp2ab

## targets produced even if they were not 
## exact matches, regardless of ungrammaticality:
exp2abo$target2<-ifelse(exp2abo$target==0 & 
                          exp2abo$synonym==1,1,
                        ifelse(exp2abo$target==1,1,0))

with(exp2abo,tapply(target2,cond,mean))
with(exp2abo,tapply(target,cond,mean))


mtargetnew<-glmer(target2~condition+
                    (1|subj.ID)+
                    (1|item.ID),
                  family=binomial(),
                  exp2abo)

mstanorig<-stan_glmer(target~condition+
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

plot(mstanorig)

mstannew<-stan_glmer(target2~condition+
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


summary(mtargetnew)
print(mstannew)
plot(mstannew)

ci95 <- posterior_interval(mstannew, prob = 0.95, pars = "condition")

samples_m1new <- as.data.frame(mstannew) 
posterior_condition <- samples_m1new$condition
options(digits = 4) 
mean(posterior_condition) 

mean(posterior_condition<0) 

## grammaticality:
with(exp2abo,tapply(gram,IND=list(cond),mean))
barplot(with(exp2abo,tapply(gram,IND=list(cond,item.ID),mean)),beside=TRUE)

barplot(with(exp2abo,tapply(gram,IND=list(cond,subj.ID),mean)),beside=TRUE)

xtabs(~gram+cond,exp2abo)

mgram<-glmer(gram~condition+
               (1|item.ID),
             family=binomial(),
             exp2abo)

summary(mgram)

mstangram<-stan_glmer(gram~condition+(1+condition|subj.ID)+(condition|item.ID),
                      family=binomial(),
                      prior_intercept=student_t(df=2),
                      prior=student_t(df=2),
                      prior_covariance=decov(regularization=2),
                      exp2abo,
                      chains=4,
                      iter=2000,
                      cores=4)

print(mstangram)


