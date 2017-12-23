library(rstan)
library(coda)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# school y se
# A  28  15
# B   8  10
# C  -3  16
# D   7  11
# E  -1   9
# F   1  11
# G  18  10
# H  12  18

# y : The observed coaching effect of each school
# se : The standard error of the coaching effect of each school.

schools_data <- list(
    J = 8,
    y = c(28,8,-3,7,-1,1,18,12),
    sigma = c(15,10,16,11,9,11,10,18)
)

model.fit <- stan(
    file = 'model/eight-schools.stan',
    data = schools_data,
    iter = 1000,
    warmup = 500,
    chains = 4,
    seed = 123456
)

print(model.fit, digits=1)
# Inference for Stan model: eight-schools.
# 4 chains, each with iter=1000; warmup=500; thin=1;
# post-warmup draws per chain=500, total post-warmup draws=2000.
#
#          mean se_mean  sd  2.5%  25%  50%  75% 97.5% n_eff Rhat
# mu        8.0     0.2 4.7  -1.2  5.1  8.0 11.1  17.6   865    1
# tau       6.6     0.3 5.9   0.3  2.4  5.2  8.9  21.9   455    1
# eta[1]    0.4     0.0 1.0  -1.7 -0.3  0.4  1.0   2.2  1309    1
# eta[2]    0.0     0.0 0.8  -1.7 -0.6  0.0  0.5   1.7  1431    1
# eta[3]   -0.2     0.0 1.0  -2.1 -0.9 -0.2  0.4   1.6  1294    1
# eta[4]    0.0     0.0 0.9  -1.8 -0.6  0.0  0.5   1.7  1472    1
# eta[5]   -0.4     0.0 0.8  -1.9 -0.9 -0.4  0.2   1.3  1695    1
# eta[6]   -0.2     0.0 0.9  -1.9 -0.8 -0.2  0.4   1.5  1319    1
# eta[7]    0.3     0.0 0.9  -1.5 -0.3  0.3  0.9   2.0  1185    1
# eta[8]    0.1     0.0 0.9  -1.8 -0.5  0.1  0.7   2.0  1393    1
# theta[1] 11.4     0.3 8.1  -1.7  6.0 10.3 15.6  31.0   872    1
# theta[2]  7.9     0.1 5.9  -4.0  4.0  8.0 11.7  20.1  1851    1
# theta[3]  6.0     0.3 8.1 -12.5  2.0  6.7 10.8  20.4   725    1
# theta[4]  7.7     0.2 6.6  -6.5  3.6  7.9 11.6  21.4  1377    1
# theta[5]  5.3     0.2 6.2  -9.0  1.6  5.7  9.5  16.5  1443    1
# theta[6]  6.3     0.2 6.3  -7.6  2.5  6.6 10.5  17.7  1356    1
# theta[7] 10.6     0.2 6.6  -0.8  6.2  9.9 14.4  26.1  1465    1
# theta[8]  8.6     0.2 8.1  -8.0  4.4  8.5 12.7  25.6  1366    1
# lp__     -4.8     0.1 2.6 -10.5 -6.4 -4.5 -2.9  -0.5   569    1

model.fit.coda <- mcmc.list(
    lapply(1:ncol(model.fit), function(x) mcmc(as.array(model.fit)[,x,]))
)

plot(model.fit.coda)
