data {
  int<lower = 1> N;                  //number of data points
  vector[N] y;                       //dep variable vot
  int<lower = 1> J;                  //number of subjects
  int<lower = 1> K;                  //number of items
  vector[J] meanvdur;                // noisy centered mean vdur  
  vector<lower = 0>[J] se;             // se mean vdur 
  int<lower = 1, upper = J> subj[N];   //subject id
  int<lower = 1, upper = K> item[N];   //item id
}

parameters {
  vector[2] beta;              //fixed intercept and slopes
  vector[J] true_mvdur; // true unknown value mvdur
  vector[J] u;                 //subject intercepts
  vector[K] w;                 //item intercepts
  real<lower=0> sigma_e;       //error sd
  real<lower=0> sigma_u;       //subj sd
  real<lower=0> sigma_w;       //item sd
}

model {
  vector[N] mu;
  //priors
  true_mvdur ~ normal(0, 200);         
  meanvdur ~ normal(true_mvdur, se); // measurement model
  beta[1] ~ normal(0, 200);
  beta[2] ~ normal(0, 5);
  sigma_e ~ normal(0, 20);
  sigma_u ~ normal(0, 20);
  sigma_w ~ normal(0, 20);
  u ~ normal(0, sigma_u);    //subj random effects
  w ~ normal(0, sigma_w);    //item random effects
  // likelihood
  mu = beta[1] + u[subj] + w[item] + beta[2] * true_mvdur[subj];
  y ~ normal(mu, sigma_e);
}
