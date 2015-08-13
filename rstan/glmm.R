require(glmmML)
require(rstan)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- as.data.frame(read.csv(file="data/data.csv"))
# > head(d)
#   N y x id
# 1 8 0 2  1
# 2 8 1 2  2
# 3 8 2 2  3
# 4 8 4 2  4
# 5 8 1 2  5
# 6 8 0 2  6

d.glmm <- glmmML(
    cbind(y, N-y) ~ x,
    family=binomial,
    cluster=id,
    data=d
  )

summary(d.glmm)
# coef se(coef)      z Pr(>|z|)
# (Intercept) -4.190   0.8777 -4.774 1.81e-06
# x            1.005   0.2075  4.843 1.28e-06
#
# Scale parameter in mixing distribution:  2.408 gaussian
# Std. Error:                              0.2202
#
# LR p-value for H_0: sigma = 0:  2.136e-55
#
# Residual deviance: 269.4 on 97 degrees of freedom 	AIC: 275.4

stan_code <- '
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
'

d.list <- list(
    N=8,
    n=100,
    x=d$x,
    y=d$y
  )

d.fit <- stan(model_code=stan_code, data=d.list, iter=1000, chains=4)
print(d.fit, digit=2)
# mean se_mean   sd    2.5%     25%     50%     75%   97.5% n_eff Rhat
# beta0    -4.20    0.06 0.98   -6.23   -4.83   -4.15   -3.55   -2.36   308 1.00
# beta1     1.01    0.01 0.23    0.58    0.85    1.01    1.16    1.50   315 1.01
# r[1]     -2.06    0.04 1.76   -5.93   -3.15   -1.88   -0.82    0.92  2000 1.00
# r[2]     -0.12    0.03 1.19   -2.58   -0.85   -0.05    0.72    2.00  1200 1.00
# r[3]      0.81    0.03 1.01   -1.31    0.17    0.88    1.48    2.69  1053 1.00
# r[4]      2.01    0.03 0.89    0.30    1.42    1.99    2.60    3.79   758 1.00
# r[5]     -0.16    0.04 1.25   -3.15   -0.82   -0.05    0.69    2.03  1108 1.00
# r[6]     -2.05    0.04 1.74   -5.69   -3.20   -1.92   -0.82    0.92  2000 1.00
# r[7]     -2.06    0.04 1.78   -6.15   -3.10   -1.91   -0.77    0.91  2000 1.00
# r[8]      3.81    0.04 1.07    1.88    3.08    3.73    4.44    6.08   746 1.00
# r[9]     -0.13    0.03 1.17   -2.66   -0.83   -0.08    0.65    1.85  1339 1.00
# r[10]     3.08    0.03 0.96    1.30    2.43    3.05    3.71    5.01   889 1.00
# r[11]    -2.08    0.04 1.81   -6.23   -3.10   -1.94   -0.83    0.77  2000 1.00
# r[12]    -2.09    0.04 1.80   -6.18   -3.22   -1.90   -0.81    0.80  2000 1.00
# r[13]    -2.07    0.04 1.82   -5.97   -3.17   -1.84   -0.78    0.88  2000 1.00
# r[14]    -2.11    0.04 1.83   -6.04   -3.24   -1.92   -0.83    0.98  2000 1.00
# r[15]    -2.09    0.04 1.84   -6.25   -3.18   -1.91   -0.80    0.99  2000 1.00
# r[16]    -0.15    0.03 1.18   -2.62   -0.90   -0.06    0.68    1.96  2000 1.00
# r[17]     1.46    0.03 0.94   -0.43    0.83    1.47    2.08    3.30   779 1.00
# r[18]     2.51    0.03 0.89    0.83    1.90    2.51    3.10    4.30   811 1.00
# r[19]     1.44    0.03 0.94   -0.52    0.85    1.47    2.08    3.21   784 1.00
# r[20]     3.80    0.04 1.10    1.86    3.05    3.71    4.49    6.20   976 1.00
# ...
# s_r       2.62    0.02 0.33    2.02    2.39    2.60    2.82    3.32   408 1.01
# lp__   -441.60    0.50 9.59 -461.04 -447.78 -441.19 -434.75 -424.00   370 1.01
