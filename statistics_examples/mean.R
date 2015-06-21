x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100)
# Sample mean
mean(x)
# [1] 14.5

# Median
median(x)
# [1] 5.5

x <- c(1.28, 1.26, 1.28, 1.40 , 1.20)
# Geometric mean
prod(x)^(1/length(x))
# [1] 1.282389

x <- c(1, 120, 3, 90, 134, 12, 50, 10)
# Harmonic mean
1/sum(1/x)*length(x)
# [1] 5.116484
