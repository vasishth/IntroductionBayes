data {
	int<lower=1> N;
	real y[N];                     //outcome
	int<lower=-1,upper=1> condition[N];  //predictor
	int<lower=1> I;                  //number of subjects
	int<lower=1> K;                  //number of items
	int<lower=1, upper=I> subject[N];   //subject id
	int<lower=1, upper=K> item[N];   //item id
}

parameters {
	vector[2] beta;			// intercept and slope
	real<lower=0> sigma_e;		// residual sd
	vector<lower=0>[2] sigma_u;	// subj sd
	vector<lower=0>[2] sigma_w;	// item sd
	cholesky_factor_corr[2] L_u;
	cholesky_factor_corr[2] L_w;
	matrix[2,I] z_u;
	matrix[2,K] z_w;
}

model {
	real mu[N]; 	// mu for likelihood
	matrix[I,2] u;	// random intercept and slopes subj
	matrix[K,2] w;
	
	# priors:
	beta ~ normal(0,10);
	sigma_e ~ normal(0,8);
	sigma_u ~ normal(0,8);
	sigma_w ~ normal(0,8);
	L_u ~ lkj_corr_cholesky(2.0);
	L_w ~ lkj_corr_cholesky(2.0);
	to_vector(z_u) ~ normal(0,1);
	to_vector(z_w) ~ normal(0,1);
	
	u <- (diag_pre_multiply(sigma_u,L_u) * z_u)';	// subj random effects
	w <- (diag_pre_multiply(sigma_w,L_w) * z_w)';	// item random effects
	
	for (n in 1:N)
		mu[n] <- beta[1] + u[subject[n],1] + w[item[n],1] + (beta[2] + u[subject[n],2] + w[item[n],2])*condition[n];
	y ~ normal(mu,sigma_e);        // likelihood
}
