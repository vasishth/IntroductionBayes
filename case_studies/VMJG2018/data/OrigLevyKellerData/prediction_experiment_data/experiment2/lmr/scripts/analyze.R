#condition dat    adj
#a         sub    sub
#b         sub    main 
#c         main   sub
#d         main   main

library(lme4)



cat('########## EXPERIMENT 2 ##########\n\n')

cat('########## REGION 8 ##########\n\n')

cat('# First fixation\n\n')
reading_time <- read.table('exp3_1fx_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region8 != 0,]
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
show(interact)

cat('\n\n# First pass\n\n')
reading_time <- read.table('exp3_1ps_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region8 != 0,]
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
show(interact)

cat('\n\n# Regression path time\n\n')
reading_time <- read.table('exp3_rpt_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region8 != 0,]
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
show(interact)

cat('\n\n# Total time\n\n')
reading_time <- read.table('../results/exp3_tt_r.res', header=TRUE)

head(reading_time)

condition<-ifelse(reading_time$dat=="sub" & reading_time$adj=="sub","a",ifelse(reading_time$dat=="sub" & reading_time$adj=="main","b",ifelse(reading_time$dat=="main" & reading_time$adj=="sub","c",
                                                                                                                                             ifelse(reading_time$dat=="main" & reading_time$adj=="main","d","NA"))))
summary(factor(condition))
reading_time$condition<-factor(condition)


reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region8 != 0,]

interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
summary(interact)

dat<-reading_time
## sliding contrasts:
dat$adj_ba<-ifelse(dat$condition%in%c("a"),-1/2,ifelse(dat$condition%in%c("b"),1/2,0))
dat$datvsadj_cb<-ifelse(dat$condition%in%c("b"),-1/2,ifelse(dat$condition%in%c("c"),1/2,0))
dat$adjdatvsdat_dc<-ifelse(dat$condition%in%c("c"),-1/2,ifelse(dat$condition%in%c("d"),1/2,0))

sliding<-lmer(region7~adj_ba + datvsadj_cb + adjdatvsdat_dc + (1+adj_ba + datvsadj_cb + adjdatvsdat_dc||subj)+(1+adj_ba + datvsadj_cb + adjdatvsdat_dc||item),subset(dat,region8>0))
summary(sliding)


cat('\n\n# Second pass\n\n')
reading_time <- read.table('../results/exp3_2ps_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region8 ~ dat*adj + (dat*adj||subj) + (dat*adj||item), data=reading_time)
summary(interact)

op<-par(mfrow=c(1,2),pty="s")
hist(reading_time$region8,freq=FALSE,main="zero RRT included")
summary(reading_time$region8)
hist(subset(reading_time,region8>0)$region8,freq=FALSE,main="zero RRT excluded")
summary(subset(reading_time,region8>0)$region8)

qqPlot(residuals(interact))

165.536-2*67.809;165.536+2*67.809

#retrodesign(165.536,67.809)

cat('\n\n# First pass regressions\n\n')
reading_time <- read.table('exp3_fpr_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time, family=binomial)
show(interact)

cat('\n\n# Skipping probability\n\n')
reading_time <- read.table('exp3_skp_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region8 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time, family=binomial)
show(interact)


cat('\n\n########## REGION 9 ##########\n\n')

cat('# First fixation\n\n')
reading_time <- read.table('exp3_1fx_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region9 != 0,]
interact  <- lmer(region9 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
show(interact)

cat('\n\n# First pass\n\n')
reading_time <- read.table('exp3_1ps_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region9 != 0,]
interact  <- lmer(region9 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
show(interact)

cat('\n\n# Regression path time\n\n')
reading_time <- read.table('exp3_rpt_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region9 != 0,]
interact  <- lmer(region9 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
show(interact)

cat('\n\n# Total time\n\n')
reading_time <- read.table('exp3_tt_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
reading_time_nozeros <- reading_time[reading_time$region9 != 0,]
interact  <- lmer(region9 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time_nozeros)
show(interact)

cat('\n\n# Second pass\n\n')
reading_time <- read.table('exp3_2ps_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region9 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time)
show(interact)

cat('\n\n# First pass regressions\n\n')
reading_time <- read.table('exp3_fpr_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region9 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time, family=binomial)
show(interact)

cat('\n\n# Skipping probability\n\n')
reading_time <- read.table('exp3_skp_r.res', header=TRUE)
reading_time$dat <- as.numeric(reading_time$dat)
reading_time$dat <- reading_time$dat - mean(reading_time$dat)
reading_time$adj <- as.numeric(reading_time$adj)
reading_time$adj <- reading_time$adj - mean(reading_time$adj)
interact  <- lmer(region9 ~ dat*adj + (dat*adj|subj) + (dat*adj|item), data=reading_time, family=binomial)
show(interact)


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
