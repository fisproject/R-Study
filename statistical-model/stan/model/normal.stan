data {
  int<lower=1> N;
  vector<lower=1>[N] x;
  vector<lower=1>[N] y;
}

parameters {
  real alpha;
  real<lower=0> e;
  real<lower=0> s;
}

model {
  for(i in 1:N)
    y[i] ~ normal(x[i] * alpha + e, s);
}
