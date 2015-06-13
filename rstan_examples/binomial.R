require(rstan)

d <- data.frame(
        N=10,
        x=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        y=c(0, 0, 1, 2, 4, 7, 8, 7, 8, 9)
  )

d.glm <- glm(
    cbind(y, N-y) ~ x,
    family=binomial(link="logit"),
    data=d
  )

summary(d.glm)
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -3.9800     0.7788  -5.110 3.21e-07 ***
# x             0.6713     0.1254   5.352 8.70e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 57.0665  on 9  degrees of freedom
# Residual deviance:  5.9736  on 8  degrees of freedom
# AIC: 29.001

stan_code <- '
  data {
    int<lower=1> N;
    int<lower=1> x[N];
    int<lower=0, upper=N> y[N];
  }

  parameters {
    real beta0;
    real beta1;
  }

  model {
    for(i in 1:N)
      y[i] ~ binomial(N, inv_logit(beta0 + beta1 * x[i]));
  }
'

d.list <- list(
    N=10,
    x=d$x,
    y=d$y
  )

d.fit <- stan(model_code=stan_code, data=d.list, iter=1000, chains=4)
print(d.fit, digit=2)
# Inference for Stan model: stan_code.
# 4 chains, each with iter=1000; warmup=500; thin=1;
# post-warmup draws per chain=500, total post-warmup draws=2000.
#
#         mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# beta0  -4.12    0.05 0.78  -5.71  -4.65  -4.06  -3.58  -2.74   232 1.02
# beta1   0.70    0.01 0.12   0.48   0.61   0.69   0.77   0.96   287 1.01
# lp__  -44.41    0.04 0.93 -46.85 -44.83 -44.13 -43.72 -43.47   492 1.00
