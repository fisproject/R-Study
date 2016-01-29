data {
  int<lower=0> N;
  real<lower=0> x[N];
}

parameters {
  real<lower=0> mu;
  real<lower=0>	sigma;
}

model {
  x ~ lognormal(mu, sigma);
}

generated quantities{
  real<lower=0>	zeta;
  real<lower=0>	lower;
  real<lower=0>	upper;
  zeta <- exp(mu);
  lower <- exp(mu - 0.95 * sigma);
  upper <- exp(mu + 0.95 * sigma);
}
