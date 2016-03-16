require(rstan)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- data.frame(
    x=c(1,2,3,4,5,6,7,8,9,10),
    y=c(9,8,10,9,11,12,11,12,14,15)
)

d.lm <- lm(y ~ x, data=d)
summary(d.lm)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  7.33333    0.60168  12.188  1.9e-06 ***
# x            0.68485    0.09697   7.062 0.000106 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.8808 on 8 degrees of freedom
# Multiple R-squared:  0.8618,	Adjusted R-squared:  0.8445
# F-statistic: 49.88 on 1 and 8 DF,  p-value: 0.0001058

d.glm <- glm(
    y ~ x,
    family = gaussian(link="identity"),
    data = d
)

summary(d.glm)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  7.33333    0.60168  12.188  1.9e-06 ***
# x            0.68485    0.09697   7.063 0.000106 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.7757576)
#
#     Null deviance: 44.9000  on 9  degrees of freedom
# Residual deviance:  6.2061  on 8  degrees of freedom
# AIC: 29.608

d.list <- list(
    N = 10,
    x = d$x,
    y = d$y
)

d.fit <- stan(
    file = 'model/normal.stan',
    data = d.list,
    iter = 1000,
    chains = 4
)
print(d.fit, digit=2)
# Inference for Stan model: stan_code.
# 4 chains, each with iter=1000; warmup=500; thin=1;
# post-warmup draws per chain=500, total post-warmup draws=2000.
#
#        mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# alpha  0.68    0.01 0.12  0.46  0.61  0.68  0.75  0.93   422 1.01
# e      7.33    0.04 0.73  5.87  6.89  7.33  7.77  8.76   423 1.00
# s      1.04    0.02 0.32  0.63  0.82  0.97  1.19  1.79   419 1.01
# lp__  -2.55    0.07 1.43 -6.44 -3.11 -2.18 -1.53 -0.97   377 1.01
