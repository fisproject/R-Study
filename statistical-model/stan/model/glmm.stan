data {
  int<lower=1> N;
  int<lower=1> n;
  int<lower=2, upper=6> x[n];
  int<lower=0, upper=N> y[n];
}

parameters {
  real beta0;
  real beta1;
  vector[n] r;
  real s_r;
}

model {
  for(i in 1:n) {
    y[i] ~ binomial(N, inv_logit(beta0 + beta1 * x[i] + r[i]));
    r[i] ~ normal(0, s_r);
  }
}
