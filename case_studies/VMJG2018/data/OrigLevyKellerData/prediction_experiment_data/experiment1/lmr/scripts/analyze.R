#condition dat    adj
#a         sub    sub
#b         sub    main 
#c         main   sub
#d         main   main 

library(lme4)

cat('########## EXPERIMENT 1 ##########\n\n')

cat('########## REGION 7 CRITICAL REGION ##########\n\n')

cat('# First fixation\n\n')
reading_time <- read.table('../results/exp1_1fx_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region7 != 0,]
interact  <- lmer(region7 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
summary(interact)

cat('\n\n# First pass\n\n')
reading_time <- read.table('exp1_1ps_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region7 != 0,]
interact  <- lmer(region7 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
summary(interact)

cat('\n\n# Regression path time\n\n')
reading_time <- read.table('exp1_rpt_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region7 != 0,]
interact  <- lmer(region7 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
summary(interact)

cat('\n\n# Total time\n\n')
reading_time <- read.table('../results/exp1_tt_r.res', header=TRUE)
head(reading_time)
condition<-ifelse(reading_time$dat=="sub" & reading_time$adj=="sub","a",ifelse(reading_time$dat=="sub" & reading_time$adj=="main","b",ifelse(reading_time$dat=="main" & reading_time$adj=="sub","c",
                                                                                                                                             ifelse(reading_time$dat=="main" & reading_time$adj=="main","d","NA"))))
summary(factor(condition))
reading_time$condition<-factor(condition)

## +/- 0.5 coding for dat:
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)

reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)

head(reading_time)

dat<-reading_time
## sliding contrasts:
dat$adj_ba<-ifelse(dat$condition%in%c("a"),-1/2,ifelse(dat$condition%in%c("b"),1/2,0))
dat$datvsadj_cb<-ifelse(dat$condition%in%c("b"),-1/2,ifelse(dat$condition%in%c("c"),1/2,0))
dat$adjdatvsdat_dc<-ifelse(dat$condition%in%c("c"),-1/2,ifelse(dat$condition%in%c("d"),1/2,0))

reading_time_nozeros <- reading_time[reading_time$region7 != 0,]

interact  <- lmer(region7 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
summary(interact)


library(car)
qqPlot(residuals(interact))

interactlog  <- lmer(log(region7) ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
summary(interactlog)

qqPlot(residuals(interactlog))


retrodesign <- function(A, s, alpha=.05, df=Inf, n.sims=10000){
  z <- qt(1-alpha/2, df)
  p.hi <- 1 - pt(z-A/s, df)
  p.lo <- pt(-z-A/s, df)
  power <- p.hi + p.lo
  typeS <- p.lo/power
  estimate <- A + s*rt(n.sims,df)
  significant <- abs(estimate) > s*z
  exaggeration <- mean(abs(estimate)[significant])/A
  return(list(power=power, typeS=typeS, exaggeration=exaggeration))
}

retrodesign(-0.05,0.01 ) # log 73%

power.t.test(d=-0.05,sd=0.01*sqrt(20),n=20,alternative="two.sided",type="one.sample")


retrodesign(102.32,39.34)  # raw 73%
retrodesign(10,39.34)  # raw 5-24%
retrodesign(50,39.34) 

power.t.test(delta=50,sd=200,type="one.sample",alternative="two.sided",power=.60)

sliding<-lmer(region7~adj_ba + datvsadj_cb + adjdatvsdat_dc + (1+adj_ba + datvsadj_cb + adjdatvsdat_dc||subj)+(1+adj_ba + datvsadj_cb + adjdatvsdat_dc||item),subset(dat,region7>0))
summary(sliding)

dat$maindat<-ifelse(dat$condition%in%c("a","b"),-1/2,1/2)

nested<-lmer(region7~adj_ba + maindat + adjdatvsdat_dc + (1+adj_ba + maindat + adjdatvsdat_dc||subj)+(1+adj_ba + maindat + adjdatvsdat_dc||item),subset(dat,region7>0))
summary(nested)

cat('\n\n# Second pass\n\n')
reading_time <- read.table('exp1_2ps_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region7 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time)
summary(interact)

cat('\n\n# First pass regressions\n\n')
reading_time <- read.table('exp1_fpr_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region7 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time, family=binomial)
summary(interact)

cat('\n\n# Skipping probability\n\n')
reading_time <- read.table('exp1_skp_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region7 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time, family=binomial)
summary(interact)


cat('\n\n########## REGION 8 ##########\n\n')

cat('# First fixation\n\n')
reading_time <- read.table('exp1_1fx_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region8 != 0,]
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
summary(interact)

cat('\n\n# First pass\n\n')
reading_time <- read.table('exp1_1ps_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region8 != 0,]
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
summary(interact)

cat('\n\n# Regression path time\n\n')
reading_time <- read.table('exp1_rpt_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region8 != 0,]
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
summary(interact)

cat('\n\n# Total time\n\n')
reading_time <- read.table('exp1_tt_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region8 != 0,]
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
summary(interact)

cat('\n\n# Second pass\n\n')
reading_time <- read.table('exp1_2ps_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time)
summary(interact)

cat('\n\n# First pass regressions\n\n')
reading_time <- read.table('exp1_fpr_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time, family=binomial)
summary(interact)

cat('\n\n# Skipping probability\n\n')
reading_time <- read.table('exp1_skp_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time, family=binomial)
summary(interact)


############################################################

# for computing confidence intervals around model predicted values
# (curtesy of Roger Levy)

# fit the model as "interact";
# we could then do (for the dat=main,adj=sub condition):

X <- c(-0.5,0.5,-0.25)
Sigma <- vcov(interact)[2:4,2:4]
SE <- sqrt(t(X) %*% Sigma %*% X)

# and then the size of the confidence interval would be

  ( qnorm(0.975) - qnorm(0.025) ) * SE

# Then we use the other three values of X

X <- c(0.5,0.5,0.25)
X <- c(0.5,-0.5,-0.25)
X <- c(-0.5,-0.5,0.25)
