## ----setup,include=FALSE,cache=FALSE,echo=FALSE--------------------------
set.seed(123)

library(knitr)
# library(coda)
# library(plyr)
# library(rjags)
library(ggplot2)
library(xtable)
library(dplyr)
library(tibble)
library(magrittr)
library(lme4)
library(brms)

library(rstan)
# library(parallel)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

library(loo)

# library(rstantools)
library(bayesplot)
library(stringr)

opts_chunk$set(fig.path='fig-', 
               fig.align='center', 
               fig.show='hold', cache=TRUE)
options(replace.assign=TRUE,show.signif.stars=FALSE)
options(replace.assign=TRUE,width=75)
opts_chunk$set(dev='postscript')

## ----myhooks, include=FALSE----------------------------------------------
# To create verbatim when highlight is FALSE
#https://stackoverflow.com/questions/24845731/knitr-asis-not-working-with-output-from-hooks

  hook_chunk = knit_hooks$get('chunk')
  knit_hooks$set(chunk = function(x, options) {
    x = hook_chunk(x, options)
    if (!options$highlight) {
      # remove all kframe's
      x = gsub('\\\\(begin|end)\\{kframe\\}', '', x)
      x= gsub('\\\\definecolor.*?color\\{fgcolor\\}','',x)
      x
    } else x
  })


## ----exampleEdat,echo=FALSE, results="hide"------------------------------
rawdatE <- read.delim("data/english.txt", stringsAsFactors = F)
## voiceless stops -1, voiced stops +1:
datE <- rawdatE %>% 
        mutate(voice = ifelse(TargetConsonant %in% 
                      c("kh", "kjh", "kwh", "th", "twh"),-1,+1),
               genderc = if_else(gender == "f",.5,-.5),
               VOT = round(msVOT,0),
               i = as.integer(as.factor(paste0(gender,subject))),
               j = as.integer(as.factor(WorldBet))) %>%
               mutate(subject = paste0(str_to_upper(gender),str_pad(i,2,pad="0"))) %>%
                 select(i, j, subject, item=WorldBet,genderfact= gender, gender = genderc, VOT, vduration=msVowel,voice) %>% arrange(i,j) %>%
        print()

datE_stops <- filter(datE, voice == -1)


## ----echo=F,eval=FALSE---------------------------------------------------
## cat("\\begin{verbatim}\n")
## datE_stops %>% select(subject, item, gender, VOT) %>%
##         as.data.frame() %>%
##         head()
## cat("...\n")
## datE_stops %>% select(subject, item, gender, VOT) %>%
##         as.data.frame() %>%
##         tail()
## cat("\\end{verbatim}\n")

## ----priors-defined, echo=F----------------------------------------------
priors_beta0 <- c(0,200)
priors_beta1 <- c(0,50)
priors_sigma_e <- c(0,100)
priors_sigma_u <- c(0,100)
priors_sigma_w <- c(0,100)

## ----lkjvisual,echo=FALSE, eval =TRUE------------------------------------
fake_data <- list(x = rnorm(30,0,1),N = 30, R = 2) 

stancode <- "
data {
  int<lower=0> N; 
  real x[N]; 
  int R;
  }
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  x ~ normal(mu,sigma);  
}
generated quantities {
  corr_matrix[R] LKJ05;
  corr_matrix[R] LKJ1;
  corr_matrix[R] LKJ2;
  corr_matrix[R] LKJ4;
  LKJ05 = lkj_corr_rng(R,.5);
  LKJ1 = lkj_corr_rng(R,1);
  LKJ2 = lkj_corr_rng(R,2);
  LKJ4 = lkj_corr_rng(R,4);
}
"

fitfake <- stan(model_code = stancode, pars = c("LKJ05","LKJ1","LKJ2","LKJ4"),
                data = fake_data, chains = 4, 
                iter = 2000)

corrs<-extract(fitfake,pars=c("LKJ05[1,2]","LKJ1[1,2]","LKJ2[1,2]","LKJ4[1,2]"))

## ----figpriors,echo=FALSE------------------------------------------------
op<-par(mfrow=c(2,3),pty="s")
par(oma = rep(0, 4), mar = c(2.7, 2.7, 0.1, 0.1), mgp = c(1.7, 0.4, 0))
b<-seq(-priors_beta0[2]*2,priors_beta0[2]*2,by=0.01)
plot(b,dnorm(b,mean=priors_beta0[1],sd=priors_beta0[2]),type="l",ylab="density", 
     xlab=expression(beta[0]),ylim=c(0, 0.0082))
plot(b,dnorm(b,mean=priors_beta1[1],sd=priors_beta1[2]),type="l",ylab="density",
     xlab=expression(beta[1]),ylim=c(0, 0.0082))
sig<-seq(0,priors_sigma_e[2]*3,by=0.01)
plot(sig,dnorm(sig,mean=priors_sigma_e[1],sd=priors_sigma_e[2]),type="l",ylab="density",
     xlab=expression(sigma[e]))
plot(sig,dnorm(sig,mean=priors_sigma_u[1],sd=priors_sigma_u[2]),type="l",ylab="density",
     xlab=expression(sigma[u[0]]))
plot(sig,dnorm(sig,mean=priors_sigma_u[1],sd=priors_sigma_u[2]),type="l",ylab="density",
     xlab=expression(sigma[w[0,1]]))
plot(density(corrs[[3]],bw=0.15),ylab="density",xlab=expression(rho[w]),xlim=c(-1,1),main="")


## ----priors-set, echo=TRUE, results="hide", highlight=FALSE--------------
library(brms)

priors <- c(set_prior("normal(0, 200)", class = "Intercept"),
                      set_prior("normal(0, 50)", class = "b", 
                                coef = "gender"),
                      set_prior("normal(0, 100)", class = "sd"),
                      set_prior("normal(0, 100)", class = "sigma"),
                      set_prior("lkj(2)", class = "cor"))


## ----loaddatamandarin,echo=FALSE, results="hide"-------------------------
rawdatM <- read.delim("data/songyuan.txt", stringsAsFactors = F)
## voiceless stops -1, voiced stops +1:
datM <- rawdatM %>% 
        mutate(voice = ifelse(TargetConsonant %in% 
                      c("t", "k"),-1,+1),
               genderc = if_else(gender == "f",.5,-.5),
               VOT = round(msVOT,0),
               i = as.integer(as.factor(paste0(gender,subject))),
               j = as.integer(as.factor(WorldBet))) %>%
               mutate(subject = paste0(str_to_upper(gender),str_pad(i,2,pad="0"))) %>%
                 select(i, j, subject, item=WorldBet, genderfact= gender, gender = genderc, VOT, vduration=msVowel,voice) %>% arrange(i,j) %>%
        print()

datM_stops <- filter(datM, voice == -1)

## ----m1Ebrms, echo=TRUE, results="hide", message=FALSE,highlight=FALSE----
m1M <- brm(formula = VOT ~ gender + (1 | subject) + (gender | item),
           data = datM_stops, family = gaussian(), prior = priors,
           iter = 2000, chains = 4, control = list(adapt_delta = 0.99))

## ----stripplots,echo=FALSE,fig.width=8,fig.height=4----------------------

op<-par(mfrow=c(1,2))
par(oma = c(1,1,0,0), mar = c(2.5, 1.9, 0.1, 0.1), mgp = c(1.7, 0.4, 0))
pchs <- c(20, 18); names(pchs) <- c("unaspirated", "aspirated")
stripchart(VOT ~ subject, datM_stops, method="jitter", vertical=T,
  pch=pchs["aspirated"], las=2, xlim=c(1, 25), at=c(1:10, 16:25),
  ylim=c(2,213), ylab="")
boxplot(VOT ~ genderfact, datM_stops, at=c(14, 12), 
  range=0, add=TRUE, yaxt="n")
stripchart(VOT ~ subject, datM[datM$voice==+1, ], method="jitter", vertical=T,
  pch=pchs["unaspirated"], col="gray", at=c(1:10, 16:25), add=T)
boxplot(VOT ~ genderfact, datM[datM$voice==+1, ], at=c(14, 12), 
  range=0, border="gray", add=TRUE, yaxt="n")
mtext("subjects ordered within gender by mean long-lag VOT", side=1, cex=0.95,outer=T)
mtext(" Mandarin", line=-1, adj=0)
mtext("VOT (ms)",side=2,outer=T)

stripchart(VOT ~ subject, datE_stops,  method="jitter", vertical=T,
  pch=pchs["aspirated"], las=2, xlim=c(1, 25), at=c(1:10, 16:25),
  ylim=c(2,213), ylab="")
boxplot(VOT ~ genderfact, datE_stops, at=c(14, 12), 
  range=0, add=TRUE, yaxt="n")
stripchart(VOT ~ subject, datE[datE$voice==+1, ], method="jitter", vertical=T,
  pch=pchs["unaspirated"], col="gray", at=c(1:10, 16:25), add=T)
boxplot(VOT ~ genderfact, datE[datE$voice==+1, ], at=c(14, 12), 
  range=0, border="gray", add=TRUE, yaxt="n")
#mtext("numbers at VOT=0 are counts of voiced tokens with lead VOT ", side=1, line=2.4, cex=0.95)
mtext(" English", line=-1, adj=0)
text(c(2, 4:10, 16, 17, 20, 21, 25), rep(0,13), 
  xtabs(~ subject, datE[datE$VOT < 0, ])[c(2, 4:10, 11, 12, 15, 16, 20)])


## ----m1Ebrmsfit,echo=FALSE,message=FALSE,warning=TRUE--------------------
m1E <- brm(formula = VOT ~ gender + (1 | subject) + (gender | item),
           data = datE_stops, family = gaussian(), prior = priors,
           iter = 2000, chains = 4, control = list(adapt_delta = 0.99))

## ----question1M,echo=FALSE,message=FALSE,warning=TRUE--------------------
m1Mlmer <- lmer(VOT ~ 1 + gender + (1 | subject)+(1 + gender| item), datM_stops)
#summary(m1M)

## ----q1Mbrm,echo=FALSE,warning=TRUE,message=FALSE,results="hide"---------
get_prior(formula = VOT  ~ 1 + gender + (1 | subject)+(1 + gender| item),
            data = datM_stops)

#summary(m1M)
m1M <- brm(formula = VOT  ~ 1 + gender + (1 | subject)+(1 +gender| item),
            data = datM_stops, 
            family = gaussian(),
            prior = priors,
            iter = 2000, 
            chains = 4,
            control = list(adapt_delta = 0.99))

priorsnocorr<- c(set_prior("normal(0,200)", class = "Intercept"),
                      set_prior("normal(0,50)", class = "b",coef="gender"),
                      set_prior("normal(0,100)", class = "sd"),
                      set_prior("normal(0,100)", class = "sigma"))

m1Mnocorr <- brm(formula = VOT  ~ 1 + gender + (1 | subject)+(1 +gender|| item),
            data = datM_stops,  
            family = gaussian(),
            prior = priorsnocorr,
            iter = 2000, 
            chains = 4,
            control = list(adapt_delta = 0.95))

## ----q1Mbrmsestimates,echo=FALSE-----------------------------------------
# ## brms estimates for reporting: 
 m1Mbrmsamples_b <- posterior_samples(m1M, "^b")
 m1Mbrmsamples_sd <- posterior_samples(m1M, "^sd")
 m1Mbrmsamples_sigma <- posterior_samples(m1M, "sigma")
 m1Mbrmsamples_cor <- posterior_samples(m1M, "^cor")
 quantilesGend<-quantile(m1Mbrmsamples_b[,2],prob=c(0.025,0.975))

# # define the relevant parameters to plot and compare
# # I use the names of brms
 rel_pars <- c("b_Intercept", "b_gender", "sd_item__Intercept", "sd_item__gender",
 "sd_subject__Intercept", "cor_item__Intercept__gender","sigma")



# effect<-c("Intercept","gender","Subject Intercept","Item Intercept","Item gender","correlation","sd")

model_params <- function(lmermodel,brmsmodel, par_names=rel_pars){

  # lmer output
  lmer_out <-c(summary(lmermodel)$coefficients[,1],
  attr(VarCorr(lmermodel)$item,"stddev"),
  attr(VarCorr(lmermodel)$subject,"stddev"),
  attr(VarCorr(lmermodel)$item,"corr")[1,2],
  attr(VarCorr(lmermodel),"sc"))

  #lmer estimates for plotting:
   lmer_out <-as.data.frame(lmer_out)
  colnames(lmer_out) <- "Estimate"
  lmer_out$`2.5%ile`<-c(summary(lmermodel)$coefficients[1,1]-2*summary(lmermodel)$coefficients[1,2],rep(NA,6))
  lmer_out$`2.5%ile`[2]<-c(summary(lmermodel)$coefficients[2,1]-2*summary(lmermodel)$coefficients[2,2])

  lmer_out$`97.5%ile`<-c(summary(lmermodel)$coefficients[1,1]+2*summary(lmermodel)$coefficients[1,2],rep(NA,6))
  lmer_out$`97.5%ile`[2]<-c(summary(lmermodel)$coefficients[2,1]+2*summary(lmermodel)$coefficients[2,2])

  lmer_out$Parameter <- rel_pars

  lmer_out$model<-"Frequentist"

  # brms output
  # store the summary of the posterior for the error bars
  gen_out <- posterior_summary(brmsmodel) %>% 
    as.data.frame %>%
    as_tibble() %>%
    rownames_to_column("Parameter") %>%
    filter(Parameter %in% rel_pars) %>% 
    mutate(model="Bayesian") %>% 
    bind_rows(lmer_out)

  gen_out$Parameter <- factor(gen_out$Parameter, levels=rel_pars)
    return(gen_out)
}

ebars1Mbrm <-  model_params(m1Mlmer, m1M)


## ----plotq1mandarin,echo=FALSE,fig.width=10,fig.height=10, message=FALSE----
# default font created problems
ggplot2::theme_set(theme_default(base_size = 14, base_family = ""))

mcmc_hist_CrCI <- function(stanfit, errorbars){
  ## save the samples as a matrix
  # post_stanfit <- as.matrix(stanfit)

  # # create histogram plot
  # plot_hist <- mcmc_hist(post_stanfit,pars =rel_pars,freq=FALSE) #+ yaxis_text(TRUE) 

  plot_hist <- stanplot(stanfit, type="hist") #+ yaxis_text(TRUE) 
  # add center of each facet for the bars
  errorbars$ycenter <- plyr::laply(ggplot_build(plot_hist)$layout$panel_ranges, function(l) {
    mean(l$y.range )
  })


  plot <- plot_hist +
    geom_errorbarh(data=filter(errorbars, model=="Bayesian"), 
                   aes(x= Estimate, y=ycenter, 
                       xmin=`2.5%ile`,xmax=`97.5%ile`,linetype="Bayesian"),size=1,height = 0) +
    geom_point(data=filter(errorbars, model=="Bayesian"), 
               aes(x=Estimate,y=ycenter,shape="Bayesian"),size=4,stroke=2) +
    geom_point(data=filter(errorbars, model=="Frequentist"), 
               aes(x=Estimate,y=ycenter * .6, shape="Frequentist"),size=4,stroke=2) + 
    geom_errorbarh(data=filter(errorbars, model=="Frequentist"), 
                   aes(x= Estimate, y=ycenter * .6, 
                       xmin=`2.5%ile`,xmax=`97.5%ile`,linetype="Frequentist"),size=1,height = 0) +
    scale_linetype_discrete(name="Model") + scale_shape_discrete(name="Model") +
    guides(linetype = guide_legend(keywidth = 3)) +  theme(legend.position=c(.5,.2))

    return(plot)
}

 
plot1Mbrm <- mcmc_hist_CrCI(m1M, ebars1Mbrm)

# The Frequentist model has some NA for the CI which causes warnings:
suppressWarnings(plot(plot1Mbrm))


## ----question1E,echo=FALSE,message=FALSE,warning=TRUE--------------------
# datE$item<-as.integer(as.factor(datE$WorldBet))
m1Elmer <- lmer(VOT ~ gender + (1 | subject) + (1 + gender | item), datE_stops)
#summary(m1Elmer)

## ----M1Eplot,echo=FALSE,fig.width=10,fig.height=10, message=FALSE--------

ebars1Ebrm <-  model_params(m1Elmer, m1E)

plot1Ebrm <- mcmc_hist_CrCI(m1E, ebars1Ebrm)

# The Frequentist model has some NA for the CI which causes warnings:
suppressWarnings(plot(plot1Ebrm))


## brms estimates for reporting:
m1Ebrmsamples_b <- posterior_samples(m1E, "^b")
m1Ebrmsamples_sd <- posterior_samples(m1E, "^sd")
m1Ebrmsamples_sigma <- posterior_samples(m1E, "sigma")
m1Ebrmsamples_cor <- posterior_samples(m1E, "^cor")
quantilesGend<-quantile(m1Ebrmsamples_b[,2],prob=c(0.025,0.975))



## ----bayesfactorlstpriors, highlight=FALSE-------------------------------
# We use the same priors as before:
prior_summary(m1M,all=F)

## ----bayesfactorlst, warning=TRUE, message=FALSE,results="hide", highlight=FALSE----
m1M <- brm(formula = VOT  ~ gender + (1 | subject) + (gender | item),
            data = datM_stops, 
            family = gaussian(), prior = priors,
            save_all_pars = TRUE,
            iter = 2000, chains = 4,
            control = list(adapt_delta = 0.99))

m0M <- update(m1M, formula = ~ .-gender)

BF10 <- bayes_factor(m1M, m0M)

## ----bayesfactor,echo=FALSE, message=FALSE, results="hide"---------------

priors_cauchy <- c(set_prior("normal(0, 200)", class = "Intercept"),
             set_prior("cauchy(0, 5)", class = "b",coef="gender"),
             set_prior("normal(0, 100)", class = "sd"),
             set_prior("normal(0, 100)", class = "sigma"),
             set_prior("lkj(2)", class = "cor"))

m1M_cauchy <- brm(formula = VOT  ~ 1 + gender + (1 | subject)+(1 +gender| item),
            data = datM_stops, 
            family = gaussian(),
            prior = priors_cauchy,
            save_all_pars = TRUE,
            iter = 2000, 
            chains = 4,
            control = list(adapt_delta = 0.95))

# m0M_cauchy<-update(m1Mbrm0, formula = ~ .-gender)

BF10M_cauchy <- bayes_factor(m1M_cauchy, m0M)

priors_N20  <- c(set_prior("normal(0, 200)", class = "Intercept"),
              set_prior("normal(0,20)", class = "b", coef="gender"),
              set_prior("normal(0, 100)", class = "sd"),
              set_prior("normal(0, 100)", class = "sigma"),
              set_prior("lkj(2)", class = "cor"))


m1M_N20 <- brm(formula = VOT  ~ 1 + gender + (1 | subject)+(1 +gender| item),
            data = datM_stops,  
            family = gaussian(),
            prior = priors_N20,
            save_all_pars = TRUE,
            iter = 2000, 
            chains = 4,
            control = list(adapt_delta = 0.95))

# nullm1Mbrm1<-update(m1Mbrm1, formula = ~ .-gender)
BF10M_N20 <- bayes_factor(m1M_N20, m0M)
#BF_bridgeMandarin1

BF10M_N50 <- BF10
m1M_N50 <- m1M


priors_N70 <-  c(set_prior("normal(0, 200)", class = "Intercept"),
              set_prior("normal(0, 70)", class = "b",coef="gender"),
              set_prior("normal(0, 100)", class = "sd"),
              set_prior("normal(0, 100)", class = "sigma"),
              set_prior("lkj(2)", class = "cor"))



m1M_N70 <- brm(formula = VOT  ~ 1 + gender + (1 | subject)+(1 +gender| item),
            data = datM_stops, 
            family = gaussian(),
            prior = priors_N70,
            save_all_pars = TRUE,
            warmup = 2000, iter = 4000, 
            chains = 4,
            control = list(adapt_delta = 0.95))


BF10M_N70 <- bayes_factor(m1M_N70, m0M)


## ----output-command,echo=TRUE,warning=TRUE,message=FALSE,results="hide",highlight=FALSE----
summary(m1E)

## ----output,echo=FALSE,warning=TRUE,message=FALSE,results="asis",echo=FALSE----
cat("\\begin{verbatim}")
outp <- capture.output(summary(m1E))
cat(paste(outp[1:4],collapse=" \n"))
cat("...")
cat(paste(outp[20:23],collapse=" \n"))
cat("...")
cat("\\end{verbatim}")

## ----m1Mtraceplot,echo=FALSE,fig.width=8,fig.height=8,warning=TRUE,message=FALSE,eval=FALSE----
## stanplot(m1M,pars="^b",type="trace")+theme(text = element_text(size=20),
##         axis.text.x = element_text(angle=90, hjust=1))

## ----m1Mppcheck,echo=FALSE,dev='pdf',message=FALSE,fig.width=3,fig.height=3----
pp_check(m1M, nsamples = 100)+
  theme(text = element_text(size=16),legend.text=element_text(size=16))

## ----missingdataexample,echo=FALSE,warning=TRUE,message=FALSE,dev='pdf',fig.width=3,fig.height=3----
datMrepeat <-datM_stops

for(i in 1:length(datMrepeat$VOT)){
  toss<-rbinom(1,n=1,prob=0.05)
  if(toss==1){datMrepeat[i,]$VOT<-0}
}


m1Mrepeat <- brm(formula = VOT  ~ gender + (1 | subject) + (gender | item),
            data =datMrepeat, 
            family = gaussian(), prior = priors,
            #save_all_pars = TRUE,
            iter = 2000, chains = 4,
            control = list(adapt_delta = 0.95))

pp_check(m1Mrepeat)+
  theme(text = element_text(size=16),legend.text=element_text(size=16))


## ----looexample,echo=FALSE,include=FALSE,warning=TRUE,message=FALSE------

# Just in case the save_all_pars is messing up something
priors <- c(set_prior("normal(0, 200)", class = "Intercept"),
                      set_prior("normal(0, 50)", class = "b", 
                                coef = "gender"),
                      set_prior("normal(0, 100)", class = "sd"),
                      set_prior("normal(0, 100)", class = "sigma"),
                      set_prior("lkj(2)", class = "cor"))

m1M <- brm(formula = VOT  ~ gender + (1 | subject) + (gender | item),
            data = datM_stops, 
            family = gaussian(), prior = priors,
            iter = 2000, chains = 4,
            control = list(adapt_delta = 0.99))

m0M <- update(m1M, formula = ~ .-gender)


loom1m0<-loo(m1M,m0M)

difflooic <- loom1m0$ic_diffs__
m1loo <- loom1m0$m1M
m0loo <- loom1m0$m0M

## ----loocode,highlight=FALSE---------------------------------------------
loo(m1M, m0M)

## ----meanvotvdurplot,echo=FALSE,fig.width=8.5,fig.height=9---------------
# meansM <- aggregate(vduration ~ subject, subset(datM,voice==1), mean)
# names(meansM)[ncol(meansM)] <- "meanvdur"
# sdvdur <- aggregate(vduration ~ subject, subset(datM,voice==1), sd)$vduration
# nvdur <- aggregate(vduration ~ subject, subset(datM,voice==1), length)$vduration
# meansM$sevdur<-sdvdur/sqrt(nvdur)

# meansM$meanVOT <- aggregate(VOT ~ subject, subset(datM,voice==-1), mean)$VOT
# sdVOT <- aggregate(VOT ~ subject, subset(datM,voice==-1), sd)$VOT
# nVOT <- aggregate(VOT ~ subject, subset(datM,voice==-1), length)$VOT
# meansM$seVOT<-sdVOT/sqrt(nVOT)



datMvoiced <- datM %>% 
              filter(voice==1) %>% 
              group_by(subject) %>%
              summarize(meanvdur= mean(vduration),sdvdur= sd(vduration), sevdur= sd(vduration)/sqrt(length(vduration))) %>%
               # mutate(cmeanvdur = scale(meanvdur, scale=FALSE)) 
              # %>%
              mutate(c_meanvdur = scale(meanvdur,scale=FALSE), cmeanvdur = scale(meanvdur), sestdvdur=sevdur/sdvdur)
# Add estimates of mean and sd for VOT in long-lag stops.
meansM <- datM_stops %>%  group_by(subject) %>%
              summarize(meanVOT= mean(VOT), seVOT = sd(VOT)/sqrt(length(VOT))) %>% right_join(datMvoiced, by="subject") 
# Do the same for the English data frame.

datEvoiced <- datE %>% 
              filter(voice==1) %>% 
              group_by(subject) %>%
              summarize(meanvdur= mean(vduration),sdvdur= sd(vduration), sevdur= sd(vduration)/sqrt(length(vduration))) %>%
             mutate(c_meanvdur = scale(meanvdur,scale=FALSE), cmeanvdur = scale(meanvdur), sestdvdur=sevdur/sdvdur) 
              # %>%
              # mutate(cmeanvdur = scale(meanvdur), sestdvdur=sevdur/sdvdur)
              
meansE <- datE_stops %>%  group_by(subject) %>%
              summarize(meanVOT= mean(VOT), seVOT = sd(VOT)/sqrt(length(VOT))) %>% right_join(datEvoiced, by="subject")

# meansE <- aggregate(vduration ~ subject, subset(datE,voice==1), mean)
# names(meansE)[ncol(meansE)] <- "meanvdur"
# sdvdur <- aggregate(vduration ~ subject, subset(datE,voice==1), sd)$vduration
# nvdur <- aggregate(vduration ~ subject, subset(datE,voice==1), length)$vduration
# meansE$sevdur<-sdvdur/sqrt(nvdur)

# meansE$meanVOT <- aggregate(VOT ~ subject, subset(datE,voice==-1), mean)$VOT
# sdVOT <- aggregate(VOT ~ subject, subset(datE,voice==-1), sd)$VOT
# nVOT <- aggregate(VOT ~ subject, subset(datE,voice==-1), length)$VOT
# meansE$seVOT<-sdVOT/sqrt(nVOT)

# Plot the mean VOT +/- 1 sd against mean vduration +/- 1 sd.
par(oma=rep(0,4), 
    mar=c(2.7, 3.3, 0.1, 0.1), 
    mgp=c(1.8, 0.4, 0), pty="s")
op<-par(mfrow=c(1,2))
plot(meanVOT ~ meanvdur, meansM, xlim=c(min(meansM$meanvdur-meansM$sevdur),max(meansM$meanvdur+meansM$sevdur)), ylim=c(min(meansM$meanVOT-meansM$seVOT),max(meansM$meanVOT+meansM$seVOT)), 
     pch=19,
  xlab="mean vowel duration after unaspirated stops (ms)",
  ylab="mean VOT in aspirated stops (ms)")
mvdur<-lm(meanVOT ~ meanvdur, meansM)
abline(mvdur,lwd=2)
arrows(x0=meansM$meanvdur-meansM$sevdur,x1=meansM$meanvdur+meansM$sevdur,y0=meansM$meanVOT,y1=meansM$meanVOT,angle=90,code=3,col="gray",length=.1)

arrows(x0=meansM$meanvdur,x1=meansM$meanvdur,y0=meansM$meanVOT-meansM$seVOT,y1=meansM$meanVOT+meansM$seVOT,angle=90,code=3,col="gray",length=.1)

mtext(" Mandarin", line=-1, adj=0)

plot(meanVOT ~ meanvdur, meansE, xlim=c(min(meansE$meanvdur-meansE$sevdur),max(meansE$meanvdur+meansE$sevdur)), ylim=c(min(meansE$meanVOT-meansE$seVOT),max(meansE$meanVOT+meansE$seVOT)), 
     pch=19,
    xlab="mean vowel duration after voiced stops (ms)",
  ylab="mean VOT in voiceless stops (ms)")
mvdurE<-lm(meanVOT ~ meanvdur, meansE)
abline(mvdurE,lwd=2)
arrows(x0=meansE$meanvdur-meansE$sevdur,x1=meansE$meanvdur+meansE$sevdur,y0=meansE$meanVOT,y1=meansE$meanVOT,angle=90,code=3,col="gray",length=.1)

arrows(x0=meansE$meanvdur,x1=meansE$meanvdur,y0=meansE$meanVOT-meansE$seVOT,y1=meansE$meanVOT+meansE$seVOT,angle=90,code=3,col="gray",length=.1)
mtext(" English", line=-1, adj=0)


meansM %<>%   select(subject, meanVOT, seVOT, c_meanvdur,sevdur)
meansE %<>%   select(subject, meanVOT, seVOT, c_meanvdur,sevdur)


## ----q2lm,echo=FALSE-----------------------------------------------------
lmMandarin <- summary(lm(meanVOT~c_meanvdur,meansM))
lmEnglish <- summary(lm(meanVOT~c_meanvdur,meansE))

## ----lmxtableMandarin,echo=FALSE,results='asis',eval=FALSE---------------
## # xtable(lmMandarin,caption="Linear model showing the effect of mean vowel duration on mean VOT in Mandarin.",label="tab:lmMandarin")

## ----lmxtableEnglish,echo=FALSE,results='asis',eval=FALSE----------------
## # xtable(lmEnglish,caption="Linear model showing the effect of mean vowel duration on mean VOT in English.",label="tab:lmEnglish")

## ----measurementerrorM,echo=TRUE,message=FALSE,warning=TRUE, highlight=FALSE----
## data frame used:
head(meansM) 

priors_cauchy <- c(set_prior("normal(0, 200)", class = "Intercept"),
            set_prior("cauchy(0, 5)", class = "b", 
                      coef = "mec_meanvdursevdur"),
            set_prior("normal(0, 20)", class = "sdme"),
            set_prior("normal(0, 20)", class = "sd"))

m2M_error <- brm(formula = meanVOT | se(seVOT)  ~ me(c_meanvdur, sevdur) + 
            (1 | subject),
            data = meansM, family = gaussian(), prior = priors_cauchy,
            iter = 2000, chains = 4,
            control = list(adapt_delta = 0.999,
                          max_treedepth=15))

## ----measurementerrorEmodel,echo=FALSE,message=FALSE,warning=TRUE, cache=FALSE, eval=TRUE----
m2E_error <- brm(formula = meanVOT | se(seVOT)  ~ 1 + me(c_meanvdur,sevdur) 
  + (1|subject),
            data = meansE, 
            family = gaussian(),
            prior = priors_cauchy,
            warmup = 1000, iter = 2000, 
            chains = 4,
            control = list(adapt_delta = 0.9999,
                          max_treedepth=15)) 

## ----q3preparedata,echo=FALSE--------------------------------------------
#colnames(Mdatmerged)
#colnames(Edatmerged)
# Mdatmerged$lang<-  1/2
# Edatmerged$lang<- -1/2
# # Mdatmerged<-Mdatmerged[,c(1,4:10)]
# # Edatmerged<-Edatmerged[,c(1,4:10)]
# Mdatmerged$subject<-paste("M",Mdatmerged$subject,sep="")
# Edatmerged$subject<-paste("E",Edatmerged$subject,sep="")
# Mdatmerged$item<-paste("M",Mdatmerged$item,sep="")
# Edatmerged$item<-paste("E",Edatmerged$item,sep="")

# datmerged<-rbind(Mdatmerged,Edatmerged)
# datmerged$gendxlang<-datmerged$gender*datmerged$lang*2

# m3a<-lmer(VOT~gender+lang+gendxlang+
#            (1|subject)+(1+gender|item),
#           datmerged)
# #summary(m3a)


# m3b<-lmer(VOT~cmeanvdur*lang+
#            (1|subject)+(1|item),
#           datmerged)
#summary(m3b)

## ----q3brmsmodelfits,echo=FALSE,warning=TRUE,include=FALSE---------------

datM_stops %<>% mutate(lang = .5, subject=paste0("M", subject))
datE_stops %<>% mutate(lang = -.5, subject=paste0("E", subject))

datmerged <- bind_rows(select(datM_stops, -i,-j),select(datE_stops, -i,-j))

priors<-c(set_prior("normal(0,200)",class = "Intercept"),
          set_prior("normal(0,50)", class = "b"),
          set_prior("normal(0,100)", class = "sd"),
          set_prior("normal(0,100)", class = "sigma")
          )

m3abrm <- brm(formula = VOT  ~ gender * lang + (1 | subject)+(1 +gender| item),
            data = datmerged, 
            family = gaussian(),
            prior = priors,
            iter = 2000, 
            chains = 4,
            control = list(adapt_delta = 0.95))

#summary(m3abrm)


datEvoiced
# %>%
              # 
              
datMvoiced %<>% mutate(lang = .5, subject=paste0("M", subject))
datEvoiced %<>% mutate(lang = -.5, subject=paste0("E", subject))

datvoiced <-  bind_rows(datMvoiced,datEvoiced) 
# %>% mutate(cmeanvdur = scale(meanvdur), sestdvdur=sevdur/sdvdur)

datmerged %<>% left_join(datvoiced)


m3bbrm <- brm(formula = VOT  ~ 1 + cmeanvdur*lang + (1 | subject)+(1 | item),
            data = datmerged, 
            family = gaussian(),
            prior = priors,
            iter = 2000, 
            chains = 4,
            control = list(adapt_delta = 0.95,max_treedepth=12))

#summary(m3bbrm)

## ----q3alatextables,echo=FALSE,results='asis',eval=TRUE------------------
summary_m3abrm <- summary(m3abrm)$fixed[,c(1,3,4)]
colnames(summary_m3abrm) <- c("Estimate","lower","upper")
print(xtable(summary_m3abrm,caption="The main effects of gender and language, and their interaction.",label="tab:m3a"))

## ----q3blatextables,echo=FALSE,results='asis',eval=TRUE------------------
summary_m3bbrm <- summary(m3bbrm)$fixed[,c(1,3,4)]
colnames(summary_m3bbrm) <- c("Estimate","lower","upper")
print(xtable(summary_m3bbrm,caption="The main effects of centered and scaled vowel duration and language, and their interaction.",label="tab:m3b"))

