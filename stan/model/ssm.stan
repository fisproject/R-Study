data {
  int<lower=1> N;
  int<lower=0, upper=6> week_day[N];
  vector<lower=0>[N] y;
}

parameters {
  vector[N] trend;
  vector[6] s_raw;
  vector[N] ar;
  real c_ar[2];
  real<lower=0> s_trend;
  real<lower=0> s_ar;
  real<lower=0> s_r;
}

transformed parameters {
  vector[7] s;
  vector[N] week;
  vector[N] y_mu;

  s[1:6] = s_raw;
  s[7] = -sum(s_raw);
  for(i in 1:N)
    week[i] = s[week_day[i]+1];
  y_mu = trend + week + ar;
}

model {
  s_trend ~ normal(0, 10);
  s_ar ~ normal(0, 10);
  s_r ~ normal(0, 20);

  trend[3:N] ~ normal(2*trend[2:N-1] - trend[1:N-2], s_trend);
  ar[3:N] ~ normal(c_ar[1]*ar[2:N-1] + c_ar[2]*ar[1:N-2], s_ar);
  y ~ normal(y_mu, s_r);
}
