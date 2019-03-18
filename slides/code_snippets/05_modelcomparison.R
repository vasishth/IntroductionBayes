## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
library(brms)

## ----echo=TRUE-----------------------------------------------------------
x<-rnorm(100)
sum(dnorm(x,mean=0,sd=1,log=TRUE))
sum(dnorm(x,mean=10,sd=1,log=TRUE))

## ------------------------------------------------------------------------
(probDataM1<-dbinom(9,p=0.5,size=10))

## ----echo=TRUE-----------------------------------------------------------
res<-(1/3)* (choose(10,9)* (0)^9 * (1-0)^1) + (1/3)* 
  (choose(10,9)* (0.5)^9 * (1-0.5)^1) + (1/3)* (choose(10,9)* (1)^9 * (1-1)^1)
res

## ----echo=TRUE-----------------------------------------------------------
0.0097/0.003

## ----echo=TRUE-----------------------------------------------------------
theta<-seq(0.1,1,by=0.1)
w<-rep(1/10,10)

prob<-rep(NA,length(w))
for(i in 1:length(theta)){
prob[i]<-(1/w[i])*choose(10,9)*theta[i]^9*(1-theta[i]^1)
}
## Likelihood for model M2 with 
## new prior on theta:
sum(prob)

## ----echo=TRUE-----------------------------------------------------------
0.0097/sum(prob)

## ----echo=TRUE-----------------------------------------------------------
1/(0.0097/sum(prob))

## ----echo=TRUE-----------------------------------------------------------
# Read data:
Winter <- c(-0.05,0.41,0.17,-0.13,0.00,-0.05,0.00,0.17,0.29,0.04,0.21,0.08,0.37,
            0.17,0.08,-0.04,-0.04,0.04,-0.13,-0.12,0.04,0.21,0.17,0.17,0.17,
            0.33,0.04,0.04,0.04,0.00,0.21,0.13,0.25,-0.05,0.29,0.42,-0.05,0.12,
            0.04,0.25,0.12)

Summer <- c(0.00,0.38,-0.12,0.12,0.25,0.12,0.13,0.37,0.00,0.50,0.00,0.00,-0.13,
            -0.37,-0.25,-0.12,0.50,0.25,0.13,0.25,0.25,0.38,0.25,0.12,0.00,0.00,
            0.00,0.00,0.25,0.13,-0.25,-0.38,-0.13,-0.25,0.00,0.00,-0.12,0.25,
            0.00,0.50,0.00)

## ------------------------------------------------------------------------
## not significant:
t.test(Winter,Summer,paired=TRUE)

## ------------------------------------------------------------------------
d <- Winter - Summer  

nsubj<-length(Winter)

                    
t.test(d)

## ----echo=TRUE-----------------------------------------------------------
d <- d / sd(d)        # standardize the paired difference of scores
ndata <- length(d)  # number of subjects

data <- list(x=d, ndata=ndata)  # to be passed on to Stan

## ----echo=TRUE-----------------------------------------------------------
model_example1 <- "
data { 
  int<lower=0> ndata;
  vector[ndata] x;
}
parameters {
  real sigmatmp;
  real delta;
} 
transformed parameters {
  real mu;
  real<lower=0> sigma;
  sigma = fabs(sigmatmp);
  mu = delta * sigma;
}
model {
  sigmatmp ~ cauchy(0, 1);
  delta ~ cauchy(0, 1);
  x ~ normal(mu, sigma);
}"

## ----echo=TRUE,message=FALSE,results="hide"------------------------------
library(rstan)
# Parameters to be monitored
parameters <- c("delta")

samples <- stan(model_code=model_example1,   
                data=data, 
                iter=20000, 
                chains=4)

# Collect posterior samples across all chains:
delta.posterior <- extract(samples,pars=parameters)$delta 

## ----echo=TRUE,fig.height=3----------------------------------------------
hist(delta.posterior,freq=FALSE,xlim=c(-3,3))
x<-seq(-3,3,by=0.01)
lines(x,dcauchy(x))

## ----echo=TRUE-----------------------------------------------------------
#BFs based on logspline fit
library(polspline) 
fit.posterior <- logspline(delta.posterior)

# 95% confidence interval:
x0 <- qlogspline(0.025,fit.posterior)
x1 <- qlogspline(0.975,fit.posterior)

posterior <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
prior     <- dcauchy(0)          # height of order-restricted prior at delta = 0
(BF01      <- posterior/prior)

## ----echo=FALSE,fig.height=3,fig.cap="\\label{fig:bfplotsavagedickey}Shown are the prior and posterior densities on delta. The null hypothesis was that delta is 0, and we see that delta=0 has a value 6 times larger under the posterior compared to the prior. This means that the evidence for the null hypothesis that delta=0 is 6 times more than the alternative."----
#============ Plot Prior and Posterior  ===========================
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
xlow  <- -3
xhigh <- 3
yhigh <- 4
Nbreaks <- 80
y       <- hist(delta.posterior, Nbreaks, plot=F)
plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=2,
     xlim=c(xlow,xhigh), ylim=c(0,yhigh), xlab=" ", ylab="Density", axes=F) 
axis(1, at = c(-4,-3,-2,-1,0,1,2,3,4), lab=c("-4","-3","-2","-1","0",
                                             "1", "2", "3", "4"))
axis(2)
mtext(expression(delta), side=1, line = 2.8, cex=2)
#now bring in log spline density estimation:
par(new=T)
plot(fit.posterior, ylim=c(0,yhigh), xlim=c(xlow,xhigh), lty=1, lwd=1, axes=F)
points(0, dlogspline(0, fit.posterior),pch=19, cex=2)
# plot the prior:
par(new=T)
plot(function( x ) dcauchy( x, 0, 1 ), xlow, xhigh, ylim=c(0,yhigh),
     xlim=c(xlow,xhigh), lwd=2, lty=1, ylab=" ", xlab = " ", axes=F) 
axis(1, at = c(-4,-3,-2,-1,0,1,2,3,4), lab=c("-4","-3","-2","-1","0",
                                             "1", "2", "3", "4"))
axis(2)
points(0, dcauchy(0), pch=19, cex=2)

## ----echo=TRUE-----------------------------------------------------------
y<-c(Winter,Summer)
#length(Winter)
n<-length(Summer)

cond<-factor(c(rep("winter",n),
        rep("summer",n)))
subject<-rep(rep(1:n),2)
dat<-data.frame(y,cond,subject)

## ------------------------------------------------------------------------
head(dat)

## ----echo=TRUE-----------------------------------------------------------
priors0 <- c(set_prior("cauchy(0, 1)", class = "Intercept"),
                      set_prior("cauchy(0, 1)", 
                                class = "sd"),
                      set_prior("cauchy(0, 1)", 
                                class = "sigma"))

priors <- c(set_prior("cauchy(0, 1)", class = "Intercept"),
                      set_prior("cauchy(0, 1)", 
                                class = "b"),
                      set_prior("cauchy(0, 1)", 
                                class = "sd"),
                      set_prior("cauchy(0, 1)", 
                                class = "sigma"))

## ----echo=TRUE,warning=FALSE,message=FALSE,results="asis",cache=TRUE-----
m_full <- brm(y ~ cond + (1|subject), 
              data = dat, 
              prior = priors, 
              sample_prior = TRUE, 
              iter = 10000,
        control=list(adapt_delta=0.99))

#summary(m_full)

## ----echo=TRUE-----------------------------------------------------------
# H0: No effect of cond
BF_brms_m <- brms::hypothesis(m_full, 
                        "condwinter = 0") 
## Evidence for NULL model vs FULL model:
BF_brms_m$hypothesis$Evid.Ratio  

## ----sensitivityanalysis,echo=TRUE,eval=FALSE----------------------------
## normalpriors <- c(set_prior("normal(0, 1)", class = "Intercept"),
##                       set_prior("normal(0, 1)",
##                                 class = "b"),
##                       set_prior("normal(0, 1)",
##                                 class = "sd"),
##                       set_prior("normal(0, 1)",
##                                 class = "sigma"))
## 
## m_full <- brm(y ~ cond + (1|subject),
##               data = dat,
##               prior = normalpriors,
##               sample_prior = TRUE,
##               iter = 10000,
##         control=list(adapt_delta=0.99))
## 
## #summary(m_full)

## ----echo=TRUE-----------------------------------------------------------
# H0: No effect of cond
BF_brms_m <- brms::hypothesis(m_full, 
                        "condwinter = 0")  
## Evidence for NULL model vs FULL model:
BF_brms_m$hypothesis$Evid.Ratio  

## ----echo=TRUE,message=FALSE,warning=FALSE,results="asis",cache=TRUE,include=FALSE----
m0<-brm(y~1+(1|subject),
        dat,prior=priors0,
        warmup=1000,
        iter=10000,
        save_all_pars = TRUE,
        control=list(adapt_delta=0.99))

m1<-brm(y~cond+(1|subject),
        dat,prior=priors,
        warmup=1000,
        save_all_pars = TRUE,
        iter=10000,
        control=list(adapt_delta=0.99))

## ----echo=TRUE,message=FALSE,warning=FALSE,results="asis"----------------
bayes_factor(m0,m1)$bf

## ----echo=TRUE,message=FALSE,warning=FALSE,results="asis"----------------
bayes_factor(m1,m0)$bf

## ------------------------------------------------------------------------
## You could define initial values, but we
## will let Stan do this:
#myinits <- list(
#  list(delta=-abs(rnorm(1,0,1)), deltaprior=-abs(rnorm(1,0,1)), sigmatmp=.1),
#  list(delta=-abs(rnorm(1,0,1)), deltaprior=-abs(rnorm(1,0,1)), sigmatmp=.2),
#  list(delta=-abs(rnorm(1,0,1)), deltaprior=-abs(rnorm(1,0,1)), sigmatmp=.3))

## ----echo=FALSE----------------------------------------------------------
# Parameters to be monitored
parameters <- c("delta")

model_example2 <- "
// One-Sample Comparison of Means
data { 
  int<lower=0> ndata;
  vector[ndata] x;
}
parameters {
  real sigmatmp;
  real<upper=0> delta;
} 
transformed parameters {
  real<lower=0> sigma;
  real mu;
  sigma = fabs(sigmatmp);
  mu = delta * sigma;
}
model {
  // Delta and sigma Come From (Half) Cauchy Distributions
  sigmatmp ~ cauchy(0, 1);
  delta ~ cauchy(0, 1);
  // Data
  x ~ normal(mu, sigma);
}"

## ----echo=TRUE,message=FALSE,results="hide"------------------------------
## samples from model:
samples <- stan(model_code=model_example2,   
                data=data, 
                #init=myinits,
                pars=parameters,
                iter=30000, 
                chains=4,
                control = list(adapt_delta = 0.99,max_treedepth=15))

# Collect posterior samples across all chains:
delta.posterior <- extract(samples)$delta

## ----fig,height=3--------------------------------------------------------
hist(delta.posterior,freq=FALSE,
     xlim=c(-3,0),main="Posterior distribution \n and prior (line)")
x<-seq(-3,0,by=0.001)
lines(x,dcauchy(x))

## ----echo=TRUE-----------------------------------------------------------
fit.posterior <- logspline(delta.posterior)

fit.posterior <- logspline(delta.posterior,ubound=0) # NB. note the bound

# 95% confidence interval:
x0 <- qlogspline(0.025,fit.posterior)
x1 <- qlogspline(0.975,fit.posterior)

posterior <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
prior     <- 2*dcauchy(0)       # height of order--restricted prior at delta = 0
(BF01      <- posterior/prior)

## ----fig.height=3,fig.cap="\\label{fig:posterior}The prior and posterior densities."----
#============ Plot Prior and Posterior  ===========================
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
xlow  <- -3
xhigh <- 0
yhigh <- 12
Nbreaks <- 80
y       <- hist(delta.posterior, Nbreaks, plot=F)
plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=2,
     xlim=c(xlow,xhigh), ylim=c(0,yhigh), xlab=" ", ylab="Density", axes=F) 
axis(1, at = c(-3,-2,-1,0), lab=c("-3","-2","-1","0"))
axis(2)
mtext(expression(delta), side=1, line = 2.8, cex=2)
#now bring in log spline density estimation:
par(new=T)
plot(fit.posterior, ylim=c(0,yhigh), xlim=c(xlow,xhigh), lty=1, lwd=1, axes=F)
points(0, dlogspline(0, fit.posterior),pch=19, cex=2)
# plot the prior:
par(new=T)
plot ( function( x ) 2*dcauchy( x, 0, 1 ), xlow, xhigh, ylim=c(0,yhigh),
       xlim=c(xlow,xhigh), lwd=2, lty=1, ylab=" ", xlab = " ", axes=F) 
axis(1, at = c(-3,-2,-1,0), lab=c("-3","-2","-1","0"))
axis(2)
points(0, 2*dcauchy(0), pch=19, cex=2)

## ----echo=TRUE-----------------------------------------------------------
x <- c(70,80,79,83,77,75,84,78,75,75,78,82,74,81,72,70,75,72,76,77)
y <- c(56,80,63,62,67,71,68,76,79,67,76,74,67,70,62,65,72,72,69,71)

n1 <- length(x)
n2 <- length(y)

# Rescale
y <- y - mean(x)
y <- y / sd(x)
x <- (x - mean(x)) / sd(x); 

data <- list(x=x, y=y, 
             n1=n1, n2=n2) 

## ------------------------------------------------------------------------
t.test(x,y)

## ----echo=TRUE-----------------------------------------------------------
model_example3 <- "
// Two-sample Comparison of Means
data { 
  int<lower=1> n1;
  int<lower=1> n2;
  vector[n1] x;
  vector[n2] y;
}
parameters {
  real mu;
  real sigmatmp;
  real delta;
} 
transformed parameters {
  real<lower=0> sigma;
  real alpha;
  sigma = fabs(sigmatmp);
  alpha = delta * sigma;
}
model {
  // Delta, mu, and sigma Come From (Half) Cauchy Distribution
  mu ~ cauchy(0, 1);
  sigmatmp ~ cauchy(0, 1);
  delta ~ cauchy(0, 1);
  // Data
  x ~ normal(mu + alpha / 2, sigma);
  y ~ normal(mu - alpha / 2, sigma);
}"

## ----echo=TRUE,message=FALSE,results="hide"------------------------------
# Parameters to be monitored
parameters <- c("delta")

# The following command calls Stan with specific options.
# For a detailed description type "?rstan".
samples <- stan(model_code=model_example3,   
                data=data, 
                iter=20000, 
                chains=4)

# Collect posterior samples across all chains:
delta.posterior <- extract(samples,pars=parameters)$delta  

## ----fig.height=3--------------------------------------------------------
hist(delta.posterior,freq=FALSE)

## ----echo=TRUE-----------------------------------------------------------
#============ BFs based on logspline fit ===========================
library(polspline) # this package can be installed from within R
fit.posterior <- logspline(delta.posterior)

# 95% confidence interval:
x0 <- qlogspline(0.025,fit.posterior)
x1 <- qlogspline(0.975,fit.posterior)

posterior <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
prior     <- dcauchy(0)         # height of  prior at delta = 0
(BF01 <- posterior/prior)

## ----fig.height=3--------------------------------------------------------
#============ Plot Prior and Posterior  ===========================
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
xlow  <- -3
xhigh <- 3
yhigh <- 2
Nbreaks <- 80
y       <- hist(delta.posterior, Nbreaks, plot=F)
plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=2,
     xlim=c(xlow,xhigh), ylim=c(0,yhigh), xlab=" ", ylab="Density", axes=F) 
axis(1, at = c(-4,-3,-2,-1,0,1,2,3,4), lab=c("-4","-3","-2","-1","0",
                                             "1", "2", "3", "4"))
axis(2)
mtext(expression(delta), side=1, line = 2.8, cex=2)
#now bring in log spline density estimation:
par(new=T)
plot(fit.posterior, ylim=c(0,yhigh), xlim=c(xlow,xhigh), lty=1, lwd=1, axes=F)
points(0, dlogspline(0, fit.posterior),pch=19, cex=2)
# plot the prior:
par(new=T)
plot ( function( x ) dcauchy( x, 0, 1 ), xlow, xhigh, ylim=c(0,yhigh),
       xlim=c(xlow,xhigh), lwd=2, lty=1, ylab=" ", xlab = " ", axes=F) 
axis(1, at = c(-4,-3,-2,-1,0,1,2,3,4), lab=c("-4","-3","-2","-1","0",
                                             "1", "2", "3", "4"))
axis(2)
points(0, dcauchy(0), pch=19, cex=2)

