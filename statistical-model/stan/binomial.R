library(rstan)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- data.frame(
    N = 10,
    x = c(1,2,3,4,5,6,7,8,9,10),
    y = c(0,0,1,2,2,4,6,6,8,9)
)

d.glm <- glm(
    cbind(y, N-y) ~ x,
    family = binomial(link="logit"),
    data = d
)

summary(d.glm)
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -4.4802     0.8666  -5.170 2.34e-07 ***
# x             0.6586     0.1273   5.173 2.31e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 49.4047  on 9  degrees of freedom
# Residual deviance:  2.2893  on 8  degrees of freedom
# AIC: 25.565
#
# Number of Fisher Scoring iterations: 4

# odds ratio
exp(d.glm$coefficients)
# (Intercept)           x
#  0.01868493  1.95680129

d.list <- list(
    N = 10,
    x = d$x,
    y = d$y
)

d.fit <- stan(
    file = 'model/binomial.stan',
    data = d.list,
    iter = 1000,
    chains = 4
)
print(d.fit, digit=2)
# Inference for Stan model: binomial.
# 4 chains, each with iter=1000; warmup=500; thin=1;
# post-warmup draws per chain=500, total post-warmup draws=2000.
#
#         mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# beta0  -4.63    0.06 0.90  -6.59  -5.19  -4.56  -3.99  -3.06   270 1.01
# beta1   0.68    0.01 0.13   0.45   0.59   0.67   0.76   0.97   257 1.01
# lp__  -43.87    0.06 1.03 -46.65 -44.29 -43.55 -43.14 -42.88   327 1.02
