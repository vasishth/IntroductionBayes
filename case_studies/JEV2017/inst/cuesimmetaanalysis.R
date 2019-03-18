## ----setup,include=FALSE,cache=FALSE,echo=FALSE--------------------------
library(knitr)
library(coda)
library(dplyr)
library(rjags)
library(ggplot2)
library(xtable)

library(rstan)
library(parallel)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

setwd('../data/')

## ----loadFunctions, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
## creates funnel plot:
set.seed(9991)
source("../R/funnelplot.R")
## creates panel-like plots:
source("../R/multiplot.R")
## plots target parameter:
source("../R/plotparameter.R")
## fits JAGS model:
source("../R/fitmodel.R")
## computes the posterior credible intervals,
## used for plotting and summarizing in table:
source("../R/GetCrI.R")
## plots posteriors of all studies:
source("../R/plotposteriors.R")
## summary of posterior parameters, with convergence stats:
source("../R/mysummary.R")
## summarizes posterior credible int for 
## table in paper:
source("../R/summarize.R")
## for appendix power curves:
source("../R/plotpower.R")

## ----loadData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
dat<-read.csv("MetaAnalysisData.csv",header=TRUE,sep=";")



## reorder by increasing y:
dat<-dat[with(dat,order(Effect)),]

# Sum contrasts for Interference Type (pro-/retroactive interference)
dat$proretro<-ifelse(dat$IntType=="pro",0.5,-0.5)

## Sliding contrasts for distractor prominence:
dat$contrOR2<-ifelse(dat$Prominence2=="subj_OR_topic",0.5,
                     ifelse(dat$Prominence2=="other",-0.5,0))
dat$contrAND2<-ifelse(dat$Prominence2=="subj_AND_topic",0.5,
                      ifelse(dat$Prominence2=="subj_OR_topic",
                             -0.5,0))

# Target-match data
Match<-subset(dat,TargetType=="Match")
# Target-mismatch data
Mismatch<-subset(dat,TargetType=="Mismatch")


## will do separate analyses on these subsets of data:
MatchNonAgrmt<-subset(dat,DepType=="nonagreement" & TargetType=="Match")
nMatchNonAgrmt<-dim(MatchNonAgrmt)[1] #12 studies

MatchAgrmt<-subset(dat,DepType=="agreement" & TargetType=="Match")
nMatchAgrmt <- dim(MatchAgrmt)[1] # 18 studies

MismatchAgrmt<-subset(dat,DepType=="agreement" & TargetType=="Mismatch")
nMismatchAgrmt<-dim(MismatchAgrmt)[1] # 13 studies

MatchReflReci<-subset(dat,DepType%in%c("refl","reci") & TargetType=="Match")
MismatchReflReci<-subset(dat,DepType%in%c("refl","reci") & TargetType=="Mismatch")
nMatchReflReci<-dim(MatchReflReci)[1] #21 studies
nMismatchReflReci<-dim(MismatchReflReci)[1] # 13 studies

## ----modelNoCovariates, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
cat("
model
    {
    for( i in 1:n)
    {
    p[i] <- 1/s[i]^2
    y[i] ~ dnorm(thetai[i],p[i])
    thetai[i] ~ dnorm(theta,prec)
    }
    ## priors for theta: 
    ## theta lies between (-1.96*100,1.96*100):
    theta ~ dnorm(0,1/100^2)
    
    ## Prior 1:
    #    prec ~ dgamma(0.001,0.001)
    #    tau.sq <- 1/prec
    #    tau <- pow(tau.sq,0.5)
    ## Prior 2:
    #tau ~ dunif(0,200) 
    #tau.sq <- tau*tau
    #prec<-1/(tau.sq)
    ## Prior 3: truncated normal
       tau ~ dnorm(0,1/10000)T(0,)
        tau.sq <- tau*tau
        prec<-1/(tau.sq)
    ## Prior 4: truncated t-distribution
    #    tau ~ dt(0,25,2)I(0,)
    #    tau.sq <- tau*tau
    #    prec<-1/(tau.sq)
    }",
     file="../vignettes/JAGSModels/RandomEffectsMetaAnalysisM1.jag" )

## ----fitModelTargetMatchAllDataNoCovariates, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
Matchdat <- list(y = Match$Effect,
            s = Match$SE,
            n = dim(Match)[1])

res<-fitmodel(d=Matchdat, 
m="../vignettes/JAGSModels/RandomEffectsMetaAnalysisM1.jag",
              track=c("tau","theta","thetai"))

## ----ExtractParametersTargetMatchAllData_noCovariates, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_allDatNoCovs_match<-summarize(res,rows = 1:2)
# mean
theta_match_allData_NoCovs <- round(summary_allDatNoCovs_match$mean[2],1)
# lower bounds of credible intervals
theta_match_allData_NoCovs_lower <- round(summary_allDatNoCovs_match$lower[2],1)
# upper bounds of credible intervals
theta_match_allData_NoCovs_upper <- round(summary_allDatNoCovs_match$upper[2],1)
# prob(b>0)
theta_match_allData_NoCovs_p <- round(summary_allDatNoCovs_match[[4]][2],2)

## ----fitModelTargetMismatchAllDataNoCovariates, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
Mismatchdat <- list(y = Mismatch$Effect,
            s = Mismatch$SE,
            n = dim(Mismatch)[1])

res<-fitmodel(d=Mismatchdat,
m="../vignettes/JAGSModels/RandomEffectsMetaAnalysisM1.jag",
              track=c("tau","theta","thetai"))

## ----ExtractParametersTargetMismatchAllData_noCovariates, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_allDatNoCovs_mismatch<-summarize(res,rows = 1:2)
# mean
theta_mismatch_allData_NoCovs <- round(summary_allDatNoCovs_mismatch$mean[2],1)
# lower bounds of credible intervals
theta_mismatch_allData_NoCovs_lower <- round(summary_allDatNoCovs_mismatch$lower[2],1)
# upper bounds of credible intervals
theta_mismatch_allData_NoCovs_upper <- round(summary_allDatNoCovs_mismatch$upper[2],1)
# prob(b>0)
theta_mismatch_allData_NoCovs_p <- round(summary_allDatNoCovs_mismatch[[4]][2],2)

## ----modelPromIntType, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
cat("
model
    {
    for( i in 1:n)
    {
    p[i] <- 1/s[i]^2
    y[i] ~ dnorm(thetai[i]+betaOR2*contrOR2[i]+
                 betaAND2*contrAND2[i]+betaPR*proretro[i],p[i])
    thetai[i] ~ dnorm(theta,prec)
    }
    ## prior for theta: 
    ## theta lies between (-1.96*100,1.96*100):
    theta ~ dnorm(0,1/100^2)

    ## prior for beta:
    betaAND2 ~ dnorm(0,1/100^2)
    betaOR2 ~ dnorm(0,1/100^2)
    betaPR ~  dnorm(0,1/100^2)

    ## Prior 1:
    #    prec ~ dgamma(0.001,0.001)
    #    tau.sq <- 1/prec
    #    tau <- pow(tau.sq,0.5)
    ## Prior 2:
    #tau ~ dunif(0,200) 
    #tau.sq <- tau*tau
    #prec<-1/(tau.sq)
    ## Prior 3: truncated normal
       tau ~ dnorm(0,1/10000)T(0,)
       tau.sq <- tau*tau
       prec<-1/(tau.sq)
    ## Prior 4: truncated t-distribution
    #    tau ~ dt(0,25,2)I(0,)
    #    tau.sq <- tau*tau
    #    prec<-1/(tau.sq)
    }",
     file="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysisPromslidingPR.jag" )

## ----fitModelTargetMatchAllData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
# 49 studies
datMatchPromslidingPR<-list(y=Match$Effect,
                     s=Match$SE,
                     n=dim(Match)[1],
                     contrOR2=Match$contrOR2,
                     contrAND2=Match$contrAND2,
                     proretro=Match$proretro)

res<-fitmodel(d=datMatchPromslidingPR,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysisPromslidingPR.jag",
              track=c("tau","theta","thetai","betaAND2","betaOR2","betaPR"))

## ----ExtractParametersTargetMatchAllData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_allDat_match<-summarize(res,rows = 1:5)
# means
theta_match_allData <- round(summary_allDat_match$mean[5],1)
proretro_match_allData <- round(summary_allDat_match$mean[3],1)
promOtherOr_match_allData <- round(summary_allDat_match$mean[2],1)
promOrAnd_match_allData <- round(summary_allDat_match$mean[1],1)
# lower bounds of credible intervals
theta_match_allData_lower <- round(summary_allDat_match$lower[5],1)
proretro_match_allData_lower <- round(summary_allDat_match$lower[3],1)
promOtherOr_match_allData_lower <- round(summary_allDat_match$lower[2],1)
promOrAnd_match_allData_lower <- round(summary_allDat_match$lower[1],1)
# upper bounds of credible intervals
theta_match_allData_upper <- round(summary_allDat_match$upper[5],1)
proretro_match_allData_upper <- round(summary_allDat_match$upper[3],1)
promOtherOr_match_allData_upper <- round(summary_allDat_match$upper[2],1)
promOrAnd_match_allData_upper <- round(summary_allDat_match$upper[1],1)
# prob(b>0)
theta_match_allData_p <- round(summary_allDat_match[[4]][5],2)
proretro_match_allData_p <- round(summary_allDat_match[[4]][3],2)
promOtherOr_match_allData_p <- round(summary_allDat_match[[4]][2],2)
promOrAnd_match_allData_p <- round(summary_allDat_match[[4]][1],2)

## ----PlotsPosteriorsTargetMatchAllData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
# these plots will be included in multiplot together with target-mismatch plots below
TMAndOr<-plotparameter(res,col=1,title="Target-Match\n Distractor Prominence (AND/OR)", xlabel='Effect size (ms)')
TMOrOther<-plotparameter(res,col=2,title="Target-Match\n Distractor Prominence (OR/other)", xlabel='Effect size (ms)')
TMproretro<-plotparameter(res,col=3,title="Target-Match\n  Interference Type (pro/retro)",  xlabel='Effect size (ms)')
TMTheta<-plotparameter(res,col=5,title="Target-Match\n  Interference (theta)")

## ----ForestplotTargetMatchAllData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE, fig.width=10,fig.height=10----
CrI<-getCrI(res)
plotposteriors(d=Match,CrI=CrI,start=6,title="Target-Match")

## ----fitModelTargetMismatchAllData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----

datMismatchPromslidingPR<-list(y=Mismatch$Effect,
                     s=Mismatch$SE,
                     n=dim(Mismatch)[1],
                     contrOR2=Mismatch$contrOR2,
                     contrAND2=Mismatch$contrAND2,
                     proretro=Mismatch$proretro)

res<-fitmodel(d=datMismatchPromslidingPR,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysisPromslidingPR.jag",
              track=c("tau","theta","thetai","betaAND2","betaOR2","betaPR"))

## ----ExtractParametersTargetMismatchAllData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_allDat_mism<-summarize(res,rows = 1:5)
# means
theta_mismatch_allData <- round(summary_allDat_mism$mean[5],1)
proretro_mismatch_allData <- round(summary_allDat_mism$mean[3],1)
promOtherOr_mismatch_allData <- round(summary_allDat_mism$mean[2],1)
promOrAnd_mismatch_allData <- round(summary_allDat_mism$mean[1],1)
# lower bounds of credible intervals
theta_mismatch_allData_lower <- round(summary_allDat_mism$lower[5],1)
proretro_mismatch_allData_lower <- round(summary_allDat_mism$lower[3],1)
promOtherOr_mismatch_allData_lower <- round(summary_allDat_mism$lower[2],1)
promOrAnd_mismatch_allData_lower <- round(summary_allDat_mism$lower[1],1)
# upper bounds of credible intervals
theta_mismatch_allData_upper <- round(summary_allDat_mism$upper[5],1)
proretro_mismatch_allData_upper <- round(summary_allDat_mism$upper[3],1)
promOtherOr_mismatch_allData_upper <- round(summary_allDat_mism$upper[2],1)
promOrAnd_mismatch_allData_upper <- round(summary_allDat_mism$upper[1],1)
# prob(b>0)
theta_mismatch_allData_p <- round(summary_allDat_mism[[4]][5],2)
proretro_mismatch_allData_p <- round(summary_allDat_mism[[4]][3],2)
promOtherOr_mismatch_allData_p <- round(summary_allDat_mism[[4]][2],2)
promOrAnd_mismatch_allData_p <- round(summary_allDat_mism[[4]][1],2)

## ----PlotsPosteriorsTargetMismatchAllData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
TMMAndOr<-plotparameter(res,col=1,title="Target-Mismatch\n Distractor Prominence (AND/OR)", xlabel='Effect size (ms)')
TMMOrOther<-plotparameter(res,col=2,title="Target-Mismatch\n Distractor Prominence (OR/other)", xlabel='Effect size (ms)')
TMMproretro<-plotparameter(res,col=3,title="Target-Mismatch\n Interference Type (pro/retro)", xlabel='Effect size (ms)')
TMMTheta<-plotparameter(res,col=5,title="Target-Mismatch\n  Interference (theta)")

## ----ForestplotTargetMismatchAllData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE, fig.width=8,fig.height=8----
CrI<-getCrI(res)
plotposteriors(d=Mismatch,CrI=CrI,start=6,title="Target-Mismatch")

## ----PosteriorsAllData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE, fig.width=8,fig.height=8----
multiplot(TMTheta,TMMTheta,
          TMproretro,TMMproretro,
          TMOrOther,TMMOrOther,
          TMAndOr,TMMAndOr,
          cols=2)

## ----InfluentialValuesTargetMismatchAllData, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
#Check influence of Wagers et al (2009) experiments:
MismatchNoWagers<-subset(Mismatch, Publication!="WagersEtAl09E2" & Publication!= "WagersEtAl09E4" &  Publication!= "WagersEtAl09E5" &  Publication!= "WagersEtAl09E3sing" & Publication!= "WagersEtAl09E3plu")
MismatchNoWagersdat<-list(y = MismatchNoWagers$Effect,
            s = MismatchNoWagers$SE,
            n = dim(MismatchNoWagers)[1],
            contrOR2=MismatchNoWagers$contrOR2,
            contrAND2=MismatchNoWagers$contrAND2,
            proretro=MismatchNoWagers$proretro)
resNoWagers<-fitmodel(MismatchNoWagersdat,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysisPromslidingPR.jag",
              track=c("tau","theta","thetai","betaAND2","betaOR2","betaPR"))

# Check influence of Wagers et al (2009) experiments together with Lago et al (2015) experiments:
MismatchNoWagersNoLago<-subset(Mismatch, Publication!="WagersEtAl09E2" & Publication!= "WagersEtAl09E4" &  Publication!= "WagersEtAl09E5" &  Publication!= "WagersEtAl09E3sing" & Publication!= "WagersEtAl09E3plu" & Publication != "LagoEtAl15E1" & Publication!="LagoEtAl15E2" & Publication != "LagoEtAl15E3a" & Publication != "LagoEtAl15E3b")
MismatchNoWagersNoLagodat<-list(y = MismatchNoWagersNoLago$Effect,
            s = MismatchNoWagersNoLago$SE,
            n = dim(MismatchNoWagersNoLago)[1],
            contrOR2=MismatchNoWagersNoLago$contrOR2,
            contrAND2=MismatchNoWagersNoLago$contrAND2,
            proretro=MismatchNoWagersNoLago$proretro)
resNoWagersNoLago<-fitmodel(MismatchNoWagersNoLagodat,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysisPromslidingPR.jag",
              track=c("tau","theta","thetai","betaAND2","betaOR2","betaPR"))

## ----ExtractParametersNoWagers, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_NoWagers<-summarize(resNoWagers,rows=1:5)
# means
theta_mismatch_noWagers <- round(summary_NoWagers$mean[5],1)
# prob(b>0)
theta_mismatch_noWagers_p <- round(summary_NoWagers[[4]][5],2)

## ----ExtractParametersNoWagersNoLago, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_NoWagersNoLago<-summarize(resNoWagersNoLago,rows=1:5)
# means
theta_mismatch_noWagersNoLago <- round(summary_NoWagersNoLago$mean[5],1)
# prob(b>0)
theta_mismatch_noWagersNoLago_p <- round(summary_NoWagersNoLago[[4]][5],2)

## ----modelIntType, include=TRUE, echo=FALSE, warning=TRUE, error=FALSE, message=TRUE----
## define model for meta-regression with interference type (pro-/retroactive) as predictor (i.e., without prominence)
cat("
model
    {
    for( i in 1:n)
    {
    p[i] <- 1/s[i]^2
    y[i] ~ dnorm(thetai[i]+ beta * pred[i],p[i])
    thetai[i] ~ dnorm(theta,prec)
    }
    ## prior for theta: 
    ## theta lies between (-1.96*100,1.96*100):
    theta ~ dnorm(0,1/100^2)

    ## prior for beta:
    beta ~ dnorm(0,1/100^2)

    ## Prior 1:
    #    prec ~ dgamma(0.001,0.001)
    #    tau.sq <- 1/prec
    #    tau <- pow(tau.sq,0.5)
    ## Prior 2:
    #tau ~ dunif(0,200) 
    #tau.sq <- tau*tau
    #prec<-1/(tau.sq)
    ## Prior 3: truncated normal
       tau ~ dnorm(0,1/10000)T(0,)
        tau.sq <- tau*tau
        prec<-1/(tau.sq)
    ## Prior 4: truncated t-distribution
    #    tau ~ dt(0,25,2)I(0,)
    #    tau.sq <- tau*tau
    #    prec<-1/(tau.sq)
    }",
     file="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysispred.jag" )

## ----fitModelTargetMatchArgVerb, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
# Non-agreement argument-verb dependencies (only Target-Match data available)
## 12 studies:
datMatchNonAgrmt<-list(y=MatchNonAgrmt$Effect,
                       s=MatchNonAgrmt$SE,
                       n=dim(MatchNonAgrmt)[1],
                       pred=MatchNonAgrmt$proretro)

res<-fitmodel(d=datMatchNonAgrmt,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysispred.jag",
         track=c("theta","thetai","beta","tau"))

## ----ExtractParametersTargetMatchArgVerb, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_ArgVerb_match<-summarize(res,rows = 1:3)
# means
theta_match_ArgVerb <- round(summary_ArgVerb_match$mean[3],1)
proretro_match_ArgVerb <- round(summary_ArgVerb_match$mean[1],1)
# lower bounds of credible intervals
theta_match_ArgVerb_lower <- round(summary_ArgVerb_match$lower[3],1)
proretro_match_ArgVerb_lower <- round(summary_ArgVerb_match$lower[1],1)
# upper bounds of credible intervals
theta_match_ArgVerb_upper <- round(summary_ArgVerb_match$upper[3],1)
proretro_match_ArgVerb_upper <- round(summary_ArgVerb_match$upper[1],1)
# prob(b>0)
theta_match_ArgVerb_p <- round(summary_ArgVerb_match[[4]][3],2)
proretro_match_ArgVerb_p <- round(summary_ArgVerb_match[[4]][1],2)

## ----PlotsPosteriorsTargetMatchArgVerb, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
NonAgrmtMatchproretro<-plotparameter(res,col=1,title="Target-Match: Arg-verb (non-agrmt)\n Interference Type (pro/retro)", xlabel='Effect size (ms)')
NonAgrmtMatchTheta<-plotparameter(res,col=3,title="Target-Match: Arg-verb (non-agrmt)\n Interference (theta)")

## ----fitModelTargetMatchAgrmt, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
# Subject-verb agreement, Target-Match configurations
## 18 studies
datMatchAgrmt<-list(y=MatchAgrmt$Effect,
                       s=MatchAgrmt$SE,
                       n=dim(MatchAgrmt)[1],
                       pred=MatchAgrmt$proretro)

res<-fitmodel(d=datMatchAgrmt,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysispred.jag",
         track=c("theta","thetai","beta","tau"),adapt=40000,iter=80000)

## ----ExtractParametersTargetMatchAgrmt, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_Agrmt_match<-summarize(d=res,rows=1:3)
# means
theta_match_Agrmt <- round(summary_Agrmt_match$mean[3],1)
proretro_match_Agrmt <- round(summary_Agrmt_match$mean[1],1)
# lower bounds of credible intervals
theta_match_Agrmt_lower <- round(summary_Agrmt_match$lower[3],1)
proretro_match_Agrmt_lower <- round(summary_Agrmt_match$lower[1],1)
# upper bounds of credible intervals
theta_match_Agrmt_upper <- round(summary_Agrmt_match$upper[3],1)
proretro_match_Agrmt_upper <- round(summary_Agrmt_match$upper[1],1)
# prob(b>0)
theta_match_Agrmt_p <- round(summary_Agrmt_match[[4]][3],2)
proretro_match_Agrmt_p <- round(summary_Agrmt_match[[4]][1],2)

## ----PlotsPosteriorsTargetMatchAgrmt, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
TMAgrmtproretro<-plotparameter(res,col=1,title="Target-Match: Agreement\n Interference Type (pro/retro)", xlabel='Effect size (ms)')
TMAgrmtTheta<-plotparameter(res,col=3,title="Target-Match: Agreement\n Interference (theta)")

## ----InfluentialValuesTargetMatchAgrmt, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----

# Influential values analysis:
# Only Franck et al 2015, Exp1 RC and Pearlmutter et al. 1999, Exp 3 report inhibitory interference for subject-verb agreement in target-match configurations. All other studies that report significant results found facilitation. The effect reported by Franck is extremely large (much larger than in any other experiment) Check whether the posterior distribution is still centered around 0 if removing Franck et al.
# Remove Franck et al (2015), Exp1 RC
MatchNoFranckAgrmt<-subset(MatchAgrmt, Publication != "FranckEtAl15E1RC")
MatchNoFranckAgrmtdat<-list(y = MatchNoFranckAgrmt$Effect,
            s=MatchNoFranckAgrmt$SE,
            n=dim(MatchNoFranckAgrmt)[1],
            pred=MatchNoFranckAgrmt$proretro)
resNoFranckAgrmt<-fitmodel(MatchNoFranckAgrmtdat,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysispred.jag",
         track=c("theta","thetai","beta","tau"),adapt=40000,iter=80000)

summary_NoFranckAgrmt<-summarize(resNoFranckAgrmt,rows=1:3)
theta_match_Agrmt_noFranck <- round(summary_NoFranckAgrmt$mean[3],1)
theta_match_Agrmt_noFranck_p <- round(summary_NoFranckAgrmt[[4]][3],2)

## ----fitModelTargetMismatchAgrmt, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
# Subject-verb agreement, Target-Mismatch configurations
#13 studies:
datMismatchAgrmt<-list(y=MismatchAgrmt$Effect,
                       s=MismatchAgrmt$SE,
                       n=dim(MismatchAgrmt)[1],
                       pred=MismatchAgrmt$proretro)

res<-fitmodel(d=datMismatchAgrmt,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysispred.jag",
         track=c("theta","thetai","beta","tau"),adapt=40000,iter=80000)

## ----ExtractParametersTargetMismatchAgrmt, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_Agrmt_mismatch<-summarize(d=res,rows=1:3)
# means
theta_mismatch_Agrmt <- round(summary_Agrmt_mismatch$mean[3],1)
proretro_mismatch_Agrmt <- round(summary_Agrmt_mismatch$mean[1],1)
# lower bounds of credible intervals
theta_mismatch_Agrmt_lower <- round(summary_Agrmt_mismatch$lower[3],1)
proretro_mismatch_Agrmt_lower <- round(summary_Agrmt_mismatch$lower[1],1)
# upper bounds of credible intervals
theta_mismatch_Agrmt_upper <- round(summary_Agrmt_mismatch$upper[3],1)
proretro_mismatch_Agrmt_upper <- round(summary_Agrmt_mismatch$upper[1],1)
# prob(b>0)
theta_mismatch_Agrmt_p <- round(summary_Agrmt_mismatch[[4]][3],2)
proretro_mismatch_Agrmt_p <- round(summary_Agrmt_mismatch[[4]][1],2)

## ----PlotsPosteriorsTargetMismatchAgrmt, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
TMMAgrmtproretro<-plotparameter(res,col=1,title="Target-Mismatch: Agreement\n Interference Type (pro/retro)", xlabel='Effect size (ms)')
TMMAgrmtTheta<-plotparameter(res,col=3,title="Target-Mismatch: Agreement\n Interference (theta)")

## ----InfluentialValuesTargetMismatchAgrmt, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
#Check influence of the Wagers et al (2009) experiments:
MismatchNoWagersAgrmt<-subset(MismatchAgrmt, Publication!="WagersEtAl09E2" & Publication!= "WagersEtAl09E4" &  Publication!= "WagersEtAl09E5" &  Publication!= "WagersEtAl09E3sing" & Publication!= "WagersEtAl09E3plu")
MismatchNoWagersAgrmtdat<-list(y = MismatchNoWagersAgrmt$Effect,
            s=MismatchNoWagersAgrmt$SE,
            n=dim(MismatchNoWagersAgrmt)[1],
            pred=MismatchNoWagersAgrmt$proretro)
resNoWagersAgrmt<-fitmodel(MismatchNoWagersAgrmtdat,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysispred.jag",
         track=c("theta","thetai","beta","tau"),adapt=40000,iter=80000)

#Check influence of the Wagers et al (2009) and Lago et al (2015) experiments:
MismatchNoWagersNoLagoAgrmt<-subset(MismatchAgrmt, Publication!="WagersEtAl09E2" & Publication!= "WagersEtAl09E4" &  Publication!= "WagersEtAl09E5" &  Publication!= "WagersEtAl09E3sing" & Publication!= "WagersEtAl09E3plu" & Publication != "LagoEtAl15E1" & Publication!="LagoEtAl15E2" & Publication != "LagoEtAl15E3a" & Publication != "LagoEtAl15E3b")
MismatchNoWagersNoLagoAgrmtdat<-list(y = MismatchNoWagersNoLagoAgrmt$Effect,
            s=MismatchNoWagersNoLagoAgrmt$SE,
            n=dim(MismatchNoWagersNoLagoAgrmt)[1],
            pred=MismatchNoWagersNoLagoAgrmt$proretro)
resNoWagersNoLagoAgrmt<-fitmodel(MismatchNoWagersNoLagoAgrmtdat,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysispred.jag",
         track=c("theta","thetai","beta","tau"),adapt=40000,iter=80000)

## ----ExtractParametersNoWagersAgrmt, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_NoWagersAgrmt<-summarize(resNoWagersAgrmt,rows=1:3)
# means
theta_mismatch_Agrmt_noWagers <- round(summary_NoWagersAgrmt$mean[3],1)
# prob(b>0)
theta_mismatch_Agrmt_noWagers_p <- round(summary_NoWagersAgrmt[[4]][3],2)

## ----ExtractParametersNoWagersNoLagoAgrmt, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_NoWagersNoLagoAgrmt<-summarize(resNoWagersNoLagoAgrmt,rows=1:3)
# means
theta_mismatch_Agrmt_noWagersNoLago <- round(summary_NoWagersNoLagoAgrmt$mean[3],1)
# prob(b>0)
theta_mismatch_Agrmt_noWagersNoLago_p <- round(summary_NoWagersNoLagoAgrmt[[4]][3],2)

## ----fitModelTargetMatchReciRefl, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
## 19 studies
datMatchReci<-list(y=MatchReflReci$Effect,
                       s=MatchReflReci$SE,
                       n=dim(MatchReflReci)[1],
                       pred=MatchReflReci$proretro)

res<-fitmodel(d=datMatchReci,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysispred.jag",
         track=c("theta","thetai","beta","tau"),adapt=40000,iter=80000)

## ----ExtractParametersTargetMatchReciRefl, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_ReciRefl_match<-summarize(d=res,rows=1:3)
# means
theta_match_ReciRefl <- round(summary_ReciRefl_match$mean[3],1)
proretro_match_ReciRefl <- round(summary_ReciRefl_match$mean[1],1)
# lower bounds of credible intervals
theta_match_ReciRefl_lower <- round(summary_ReciRefl_match$lower[3],1)
proretro_match_ReciRefl_lower <- round(summary_ReciRefl_match$lower[1],1)
# upper bounds of credible intervals
theta_match_ReciRefl_upper <- round(summary_ReciRefl_match$upper[3],1)
proretro_match_ReciRefl_upper <- round(summary_ReciRefl_match$upper[1],1)
# prob(b>0)
theta_match_ReciRefl_p <- round(summary_ReciRefl_match[[4]][3],2)
proretro_match_ReciRefl_p <- round(summary_ReciRefl_match[[4]][1],2)

## ----PlotsPosteriorsTargetMatchReciRefl, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
TMReciproretro<-plotparameter(res,col=1,title="Target-Match: Refl/Reci\n Interference Type (pro/retro)", xlabel='Effect size (ms)')
TMReciTheta<-plotparameter(res,col=3,title="Target-Match: Refl/Reci\n Interference (theta)")

## ----fitModelTargetMismatchReciRefl, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
# Reciprocal/reflexive-antecedent dependencies, Target-Mismatch configurations
## 11 studies
datMismatchReci<-list(y=MismatchReflReci$Effect,
                       s=MismatchReflReci$SE,
                       n=dim(MismatchReflReci)[1],
                       pred=MismatchReflReci$proretro)

res<-fitmodel(d=datMismatchReci,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysispred.jag",
         track=c("theta","thetai","beta","tau"),adapt=40000,iter=80000)

## ----ExtractParametersTargetMismatchReciRefl, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
summary_ReciRefl_mismatch<-summarize(d=res,rows=1:3)
# means
theta_mismatch_ReciRefl <- round(summary_ReciRefl_mismatch$mean[3],1)
proretro_mismatch_ReciRefl <- round(summary_ReciRefl_mismatch$mean[1],1)
# lower bounds of credible intervals
theta_mismatch_ReciRefl_lower <- round(summary_ReciRefl_mismatch$lower[3],1)
proretro_mismatch_ReciRefl_lower <- round(summary_ReciRefl_mismatch$lower[1],1)
# upper bounds of credible intervals
theta_mismatch_ReciRefl_upper <- round(summary_ReciRefl_mismatch$upper[3],1)
proretro_mismatch_ReciRefl_upper <- round(summary_ReciRefl_mismatch$upper[1],1)
# prob(b>0)
theta_mismatch_ReciRefl_p <- round(summary_ReciRefl_mismatch[[4]][3],2)
proretro_mismatch_ReciRefl_p <- round(summary_ReciRefl_mismatch[[4]][1],2)

## ----PlotsPosteriorsTargetMismatchReciRefl, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
TMMReciproretro<-plotparameter(res,col=1,title="Target-Mismatch: Refl/Reci\n Interference Type (pro/retro)", xlabel='Effect size (ms)')
TMMReciTheta<-plotparameter(res,col=3,title="Target-Mismatch: Refl/Reci\n Interference (theta)")

## ----InfluentialValuesTargetMismatchRefl, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----

#### Influential values analysis:
#Is the inhibition in target-mismatch configurations in reflexives/reciprocals driven by the Chinese data from Jaeger et al (2015), Exp. 1?
MismatchNoJaegerRefl<-subset(MismatchReflReci, Publication != "JaegerEtAl15E1")
MismatchNoJaegerRefldat<-list(y = MismatchNoJaegerRefl$Effect,
            s=MismatchNoJaegerRefl$SE,
            n=dim(MismatchNoJaegerRefl)[1],
            pred=MismatchNoJaegerRefl$proretro)
resNoJaegerRefl<-fitmodel(MismatchNoJaegerRefldat,
              m="../vignettes/JAGSModels/RandomEffectsMetaRegressionAnalysispred.jag",
         track=c("theta","thetai","beta","tau"),adapt=40000,iter=80000)

summary_resNoJaegerRefl<-summarize(resNoJaegerRefl,rows=1:3)
theta_mismatch_Refl_noJaeger <- round(summary_resNoJaegerRefl$mean[3],1)
theta_mismatch_Refl_noJaeger_p <- round(summary_resNoJaegerRefl[[4]][3],2)

## ----WriteSummaryToFile, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
	save(summary_allDat_match, summary_allDat_mism,summary_ArgVerb_match,summary_Agrmt_match,summary_Agrmt_mismatch,summary_ReciRefl_match,summary_ReciRefl_mismatch, file="SummaryMetaAnalysis.Rd")

## ----PosteriorsbyDepType, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE, fig.width=8,fig.height=8----
multiplot(NonAgrmtMatchTheta,
          NonAgrmtMatchproretro,
          TMAgrmtTheta,
          TMAgrmtproretro,
          TMMAgrmtTheta,
          TMMAgrmtproretro,
          TMReciTheta,
          TMReciproretro,
          TMMReciTheta,
          TMMReciproretro,
          cols=2)

## ----funneplots, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE, fig.width=7,fig.height=5----
sig<-ifelse(abs(Match$Effect/Match$SE)>2,"sig","ns")
Match$sig<-factor(sig)
sig<-ifelse(abs(Mismatch$Effect/Mismatch$SE)>2,"sig","ns")
Mismatch$sig<-factor(sig)

funnelmatch<-funnelplot(d=Match, title="Target-Match")
funnelmismatch<-funnelplot(d=Mismatch,
                           title="Target-Mismatch")
multiplot(funnelmatch,funnelmismatch,cols=2)

## ----stanfitagrmtattrn,echo=FALSE----------------------------------------
## estimates of interference in subj-verb agreement (target-mismatch) and their SEs extracted from LMMs:
## generalize this code:

mismatch_PhillipsLab<-subset(Mismatch,Publication%in%c("DillonEtAl13E1agrmt",
                              "LagoEtAl15E1","LagoEtAl15E2",
                              "LagoEtAl15E3a","LagoEtAl15E3b",
                              "WagersEtAl09E2","WagersEtAl09E3sing",
                              "WagersEtAl09E3plu","WagersEtAl09E4",
                              "WagersEtAl09E5"))

## sanity check
#sort(mismatch_PhillipsLab$Publication)
## number of studies:
n<-length(mismatch_PhillipsLab$Publication)

PL_data<-data.frame(study=1:n,effect=mismatch_PhillipsLab$Effect,
                    se=mismatch_PhillipsLab$SE)

PLdat<-list(n=dim(PL_data)[1],y=PL_data$effect,s=PL_data$se)

output <- stanc("../vignettes/StanModels/rema2.stan")

fit <- stan(file='../vignettes/StanModels/rema2.stan', 
            data=PLdat,
            iter=4000, chains=4, seed=987654321,
            control = list(adapt_delta = 0.999))

paramnames<-c("mu","theta")
#print(fit,pars=paramnames)

params<-extract(fit,pars=paramnames)

postquant<-round(quantile(params$mu,probs=c(0.025,0.5,0.975)))
postmean<-round(mean(params$mu))

#stan_plot(fit,pars=paramnames)
#stan_hist(fit,pars=paramnames)

## ----powercalculation,echo=FALSE-----------------------------------------
## to-do: look at all possibilities?
best<-round(power.t.test(d=postquant[1],power=0.80,sd=175,
                             type="one.sample",
               alternative="two.sided")$n)
secondbest<-round(power.t.test(d=postquant[1],power=0.80,sd=250,
                             type="one.sample",
               alternative="two.sided")$n)
worst<-round(power.t.test(d=postquant[3],power=0.80,sd=250,
                             type="one.sample",
               alternative="two.sided")$n)
intermediate<-round(power.t.test(d=postmean,power=0.80,sd=175,
                             type="one.sample",
               alternative="two.sided")$n)

## ----LoadRTMetaAnalysis,echo=FALSE---------------------------------------
load("SDsMetaAnalysis.Rda")

SDs<-SDsMetaAnalysis$SDs
precisions<-1/SDs^2
mu<-mean(SDs)
sigma<-sd(SDs)
mu_prec<-mean(precisions)
sigma_prec<-sd(precisions)

x<-seq(0,0.0005,by=0.000000001)
beta_prec<-sigma_prec^2/mu_prec
alpha_prec<-mu_prec/beta_prec
#beta_prec
#alpha_prec
SDsquants<-quantile(SDs,probs=c(0.025,0.975),na.rm=T)

## ----powerfunctionplots, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE, fig.width=8,fig.height=10----
subjects<-seq(20,120,by=20)
rows<-2
cols<-3
op<-par(mfrow=c(rows,cols),pty="s")

for(nsubj in subjects){
plotpower(nsubj=nsubj,
          stddevquantiles=c(100,300),
          posteriorquantiles=postquant,
          posteriormeans=postmean,
          mytitle=paste(nsubj,"participants",sep=" "))
}

## ----echo=FALSE,fig.width=7,fig.height=5,cache=FALSE---------------------
nsubj<-seq(30,60,by=1)

low_powermismatch<-power.t.test(d=postquant[3],sd=175,n=nsubj,type="one.sample",
             alternative="two.sided")$power

mid_powermismatch<-power.t.test(d=postmean,sd=175,n=nsubj,type="one.sample",
             alternative="two.sided")$power
high_powermismatch<-power.t.test(d=postquant[1],sd=175,n=nsubj,type="one.sample",
             alternative="two.sided")$power

low_powermismatch2<-power.t.test(d=postquant[3],sd=250,n=nsubj,type="one.sample",
             alternative="two.sided")$power

mid_powermismatch2<-power.t.test(d=postmean,sd=250,n=nsubj,type="one.sample",
             alternative="two.sided")$power
high_powermismatch2<-power.t.test(d=postquant[1],sd=250,n=nsubj,type="one.sample",
             alternative="two.sided")$power

op<-par(mfrow=c(1,2),pty="s")
plot(nsubj,low_powermismatch,ylim=c(0,.6),type="l",lty=1,
     main="sd=175 ms",xlab="number of participants",
     ylab="power")
text(45,0.1,as.character(postquant[3]))
lines(nsubj,mid_powermismatch,type="l",lty=1)
text(45,0.2,as.character(postmean))
lines(nsubj,high_powermismatch,type="l",lty=1)
text(45,0.35,as.character(postquant[1]))

plot(nsubj,low_powermismatch2,ylim=c(0,.6),type="l",lty=1,
     main="sd=250 ms",xlab="number of participants",
     ylab="power")
text(45,0.06,as.character(postquant[3]))

lines(nsubj,mid_powermismatch2,type="l",lty=1)
text(45,0.14,as.character(postmean))

lines(nsubj,high_powermismatch2,type="l",lty=1)
text(45,0.2,as.character(postquant[1]))

