require(rstan)

d <- data.frame(
        N=10,
        x=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        y=c(0, 0, 0, 1, 0, 1, 1, 1, 1, 1)
  )

d.glm <- glm(
    cbind(y, N-y) ~ x,
    family=binomial(link="logit"),
    data=d
  )

summary(d.glm)
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -4.4209     1.2998  -3.401 0.000671 ***
# x             0.2607     0.1699   1.535 0.124871
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 6.3835  on 9  degrees of freedom
# Residual deviance: 3.6563  on 8  degrees of freedom
# AIC: 19.035

stan_code <- '
  data {
    int<lower=1> N;
    int<lower=1, upper=N> x[N];
    int<lower=0, upper=1> y[N];// binary class
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
# beta0  -4.77    0.09 1.51  -8.54  -5.59  -4.52  -3.74  -2.35   301 1.04
# beta1   0.29    0.01 0.19  -0.05   0.16   0.27   0.41   0.72   320 1.03
# lp__  -22.39    0.06 1.05 -25.34 -22.79 -22.06 -21.62 -21.35   339 1.02
#
