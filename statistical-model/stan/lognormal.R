library(rstan)
library(coda)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- read.csv("data/income.csv", header=T)

d.list <- list(
    N=length(d$income),
    x=d$income
)

d.fit <- stan(
    file = 'model/lognormal.stan',
    data = d.list,
    iter = 2000,
    chains = 2
)

print(d.fit, digits=2)
# Inference for Stan model: 0cea012b87f69ca2147d3277f751c103.
# 2 chains, each with iter=2000; warmup=1000; thin=1;
# post-warmup draws per chain=1000, total post-warmup draws=2000.
#
#         mean se_mean     sd   2.5%    25%    50%    75%   97.5% n_eff Rhat
# mu      5.96    0.00   0.14   5.68   5.87   5.96   6.05    6.24   957    1
# sigma   0.76    0.00   0.11   0.59   0.69   0.75   0.82    1.01   901    1
# zeta  390.67    1.80  55.74 294.30 354.53 387.36 423.53  512.30   958    1
# lower 190.63    1.01  32.12 127.42 169.05 191.01 211.23  254.20  1005    1
# upper 808.97    4.95 149.47 590.30 708.35 784.10 882.76 1163.49   910    1
# lp__   -4.38    0.04   1.05  -7.31  -4.76  -4.04  -3.65   -3.41   589    1

model.fit.coda <- mcmc.list(
    lapply(1:ncol(d.fit), function(x) mcmc(as.array(d.fit)[,x,]))
)
plot(model.fit.coda)

ex <- extract(d.fit)
ex.zeta <-mean(ex$zeta)
ex.lower <-mean(ex$lower)
ex.upper <-mean(ex$upper)

p <- ggplot(d, aes(x = index, y = income))

p <- p + geom_line(size=0.5) +
    geom_hline(yintercept = as.integer(ex.zeta), colour = "red", linetype = "longdash") +
    geom_hline(yintercept = as.integer(ex.lower), colour = "red", linetype = "longdash", alpha = 0.5) +
    geom_hline(yintercept = as.integer(ex.upper), colour = "red", linetype = "longdash", alpha = 0.5) +
    labs(title = "", x = "Index", y = "Income")
plot(p)
