x <- c(100, 95, 80, 90, 120, 50, 95, 80, 90, 140, 100, 55, 80, 70, 90)
n <- length(x)

# unbiased_sample_variance
uvar <- var(x)

# two-side-t-value * std.err
v <- qt(0.975, n-1) * sqrt(uvar/n)
# [1] 12.52365

# under
mean(x) - v
# [1] 76.47635

# sample mean
mean(x)
# [1] 89

# upper
mean(x) + v
# [1] 101.5237
