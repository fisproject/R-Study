data {
  int<lower=1> N;
  int<lower=1> x[N];
  int<lower=1> y[N];
}

parameters {
  real beta0;
  real beta1;
}

model {
  for(i in 1:N)
    y[i] ~ poisson_log(beta0 + beta1 * x[i]);
}
