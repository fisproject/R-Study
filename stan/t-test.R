library(rstan)
library(dplyr)
library(coda)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- sleep
head(d)
# extra group ID
# 1   0.7     1  1
# 2  -1.6     1  2
# 3  -0.2     1  3
# 4  -1.2     1  4
# 5  -0.1     1  5
# 6   3.4     1  6

grp1 <- d %>% filter(group == 1)
grp2 <- d %>% filter(group == 2)

d.list <- list(
    N = nrow(d)/2,
    grp1 = grp1$extra,
    grp2 = grp2$extra
)

model.fit <- stan(
    file = 'model/t-test.stan',
    data = d.list,
    iter = 1000,
    chains = 4,
    seed = 123456
)

print(model.fit, digits=1)
# Inference for Stan model: t-test.
# 4 chains, each with iter=1000; warmup=500; thin=1;
# post-warmup draws per chain=500, total post-warmup draws=2000.
#
#       mean se_mean  sd  2.5%  25%  50%  75% 97.5% n_eff Rhat
# sigma  1.5     0.0 0.5   0.9  1.2  1.4  1.6   2.6   493    1
# mu     1.6     0.0 0.5   0.7  1.3  1.6  1.9   2.5   704    1
# lp__  -7.5     0.1 1.1 -10.4 -7.9 -7.1 -6.7  -6.4   447    1
#
# Samples were drawn using NUTS(diag_e) at Wed Mar 16 21:24:57 2016.
# For each parameter, n_eff is a crude measure of effective sample size,
# and Rhat is the potential scale reduction factor on split chains (at
# convergence, Rhat=1).

model.fit.coda <- mcmc.list(
    lapply(1:ncol(model.fit), function(x) mcmc(as.array(model.fit)[,x,]))
)

plot(model.fit.coda)
