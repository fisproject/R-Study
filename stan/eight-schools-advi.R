require(rstan)
require(ggplot2)

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
    J=8,
    y=c(28,8,-3,7,-1,1,18,12),
    sigma=c(15,10,16,11,9,11,10,18)
)

stan_model <- stan_model(file='model/eight-schools.stan')

model.fit <- vb(
    stan_model,
    data=schools_data,
    output_samples=2000, # (iter - warmup) / thin * chains
    seed=123456,
    algorithm="meanfield"
)

print(model.fit, digits=1)
# Inference for Stan model: eight-schools.
# 1 chains, each with iter=2000; warmup=0; thin=1;
# post-warmup draws per chain=2000, total post-warmup draws=2000.
#
#          mean  sd 2.5%  25%  50%  75% 97.5%
# mu        7.8 4.1 -0.4  5.1  7.6 10.5  15.7
# tau       4.1 3.0  0.9  2.1  3.3  5.2  12.4
# eta[1]    0.5 1.0 -1.6 -0.2  0.5  1.2   2.5
# eta[2]    0.1 1.0 -1.8 -0.5  0.1  0.8   2.0
# eta[3]   -0.1 1.0 -2.1 -0.8 -0.1  0.6   1.9
# eta[4]    0.1 0.9 -1.7 -0.5  0.1  0.7   1.9
# eta[5]   -0.3 0.9 -2.2 -0.9 -0.3  0.3   1.4
# eta[6]   -0.2 0.9 -2.0 -0.8 -0.1  0.4   1.7
# eta[7]    0.3 0.9 -1.5 -0.3  0.3  1.0   2.2
# eta[8]    0.1 1.0 -1.8 -0.6  0.1  0.7   2.0
# theta[1]  9.8 6.8 -3.1  5.6  9.3 13.7  23.7
# theta[2]  8.3 6.3 -4.3  4.5  8.1 12.0  21.7
# theta[3]  7.2 6.7 -6.4  3.5  7.3 11.1  19.9
# theta[4]  8.3 6.1 -3.9  4.7  8.1 11.9  20.1
# theta[5]  6.5 6.0 -5.6  3.1  6.7 10.0  17.6
# theta[6]  7.0 6.5 -5.9  3.4  7.1 10.7  18.9
# theta[7]  9.1 6.2 -2.5  5.5  9.0 12.6  21.8
# theta[8]  7.9 6.4 -5.0  4.2  8.0 11.5  20.4
# lp__      0.0 0.0  0.0  0.0  0.0  0.0   0.0

# plot parameters
samples <- data.frame(model.fit@sim$samples)
me <- lapply(samples, mean)
pt <- lapply(samples, function(x) quantile(x, c(.025, .975)))
df <- rbind(data.frame(me), data.frame(pt))
rownames(df) <- c("mean", "lower", "upper")
dft <- data.frame(t(df)) # transposed

p <- ggplot(dft, aes(x=rownames(dft), y=mean))
p <- p + geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    labs(title="parameters of eight-schools (ADVI, seed=123456)", x="param", y="value")
plot(p)
