data {
	int<lower=0> n; //number of studies
	real y[n]; // estimated effect
	real<lower=0> s[n]; // SEs of effect
}
parameters{
	real mu; //population mean
	real<lower=0> tau;  // between study variability	
	vector[n] eta; //study level errors
} 
transformed parameters {
	vector[n] theta;   // study effects
	theta = mu + tau*eta;
}
model {
eta ~ normal(0,1);
y ~ normal(theta,s);
}
generated quantities{
vector[n] y_tilde;
for(i in 1:n){
  y_tilde[i] = normal_rng(theta[i],s[i]); 
  }
}
