data {
	int<lower=0> N; //number of studies
	real y[N]; // estimated effect
	real<lower=0> sigma[N]; // SEs of effect
}
parameters{
	real theta; //population mean
	real theta_i[N]; //separate mean for each study
	real<lower=0> tau;  // between study variability
} 
model {
//priors
theta ~ normal(0,100);
theta_i ~ normal(0,100);
tau ~ cauchy(0,1);

for(i in 1:N){	
y[i] ~ normal(theta_i[i],sigma[i]);
theta_i[i] ~ normal(theta,tau);
}
}
