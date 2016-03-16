data {
	int<lower=0> N;
	real<upper=10> grp1[N];
	real<upper=10> grp2[N];
}

parameters {
	real<lower=0> sigma;
	real mu;
}

model {
	for (i in 1:N)
		grp2[i] ~ normal(grp1[i] + mu, sigma);
}
