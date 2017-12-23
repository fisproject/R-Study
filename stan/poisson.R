library(rstan)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- data.frame(
    x = c(1,2,3,4,5,6,7,8,9,10),
    y = c(44,32,26,20,14,9,7,4,2,1)
)

d.glm <- glm(
    y ~ x,
    family = poisson(link="log"),
    data = d
)

summary(d.glm)
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  4.19000    0.13638  30.723   <2e-16 ***
# x           -0.33717    0.03535  -9.537   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for poisson family taken to be 1)
#
#     Null deviance: 118.9260  on 9  degrees of freedom
# Residual deviance:   2.6949  on 8  degrees of freedom
# AIC: 47.803

d.list <- list(
    N = 10,
    x = d$x,
    y = d$y
)

d.fit <- stan(
    file = 'model/poisson.stan',
    data = d.list,
    iter = 1000,
    chains = 4
)
print(d.fit, digit = 2)
# Inference for Stan model: stan_code.
# 4 chains, each with iter=1000; warmup=500; thin=1;
# post-warmup draws per chain=500, total post-warmup draws=2000.
#
#         mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# beta0   4.18    0.01 0.14   3.90   4.09   4.19   4.28   4.46   514 1.00
# beta1  -0.34    0.00 0.04  -0.41  -0.36  -0.34  -0.31  -0.27   477 1.00
# lp__  337.93    0.05 1.10 335.03 337.57 338.24 338.66 338.94   430 1.01
