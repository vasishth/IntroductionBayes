data {
  int<lower=0> N;           
  vector[N] rt;                
  int<lower=0> J;              
  int<lower=0> K;            
  int<lower=1,upper=J> subj[N]; 
  int<lower=1,upper=K> item[N]; 
  int<lower=1> P;           
  row_vector[P] X[N];           
  int<lower=1> n_u;          
  int<lower=1> n_w;             
  row_vector[n_u] Z_u[N];       
  row_vector[n_w] Z_w[N];       
  
}

parameters {
  vector[P] beta;           
  real<lower=0> sigma_e;       
  vector<lower=0>[n_u] sigma_u; 
  vector<lower=0>[n_w] sigma_w; 
  cholesky_factor_corr[n_u] L_u;  
  cholesky_factor_corr[n_w] L_w;  
  vector[n_u] z_u[J];         
  vector[n_w] z_w[K];         
}


transformed parameters {
  vector[n_u] u[J];             
  vector[n_w] w[K];            
  {
    matrix[n_u,n_u] Sigma_u; 
    matrix[n_w,n_w] Sigma_w;  
    Sigma_u = diag_pre_multiply(sigma_u,L_u); 
    Sigma_w = diag_pre_multiply(sigma_w,L_w);
    for(j in 1:J)               
    u[j] = Sigma_u * z_u[j];   
    for(k in 1:K)
      w[k] = Sigma_w * z_w[k];  
  }
}


model {
  //priors (vaguely informative)
  beta[1] ~ normal(0,10);
  beta[2] ~ normal(0,1);
  sigma_e ~ normal(0,1);
  sigma_u ~ normal(0,1);
  sigma_w ~ normal(0,1);
  L_u ~ lkj_corr_cholesky(2.0);  
  L_w ~ lkj_corr_cholesky(2.0);
  for (j in 1:J)                
  z_u[j] ~ normal(0,1);
  for (k in 1:K)
    z_w[k] ~ normal(0,1);
  
  //likelihood
  for (i in 1:N)
    rt[i] ~ lognormal(X[i] * beta + Z_u[i] * u[subj[i]] + Z_w[i] * w[item[i]], sigma_e);
}

generated quantities {
  real Int;
  Int = (exp(beta[1] + beta[2]) - exp(beta[1] - beta[2]));
}
