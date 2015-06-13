require(rstan)

d <- data.frame(
        x=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        y=c(1, 2, 4, 7, 9, 14, 20, 26, 32, 44)
  )

d.glm <- glm(
    y ~ x,
    family=poisson(link="log"),
    data=d
  )

summary(d.glm)
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  0.48108    0.28903   1.664    0.096 .
# x            0.33717    0.03535   9.537   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for poisson family taken to be 1)
#
#     Null deviance: 118.9260  on 9  degrees of freedom
# Residual deviance:   2.6949  on 8  degrees of freedom
# AIC: 47.803

stan_code <- '
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
# beta0   0.51    0.02 0.28  -0.03   0.33   0.51   0.69   1.07   322 1.00
# beta1   0.33    0.00 0.03   0.26   0.31   0.33   0.36   0.40   324 1.00
# lp__  337.99    0.06 0.99 335.37 337.58 338.30 338.70 338.94   294 1.01
