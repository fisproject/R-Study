d <- matrix(c(2, 10, 15, 3), nrow=2,
           dimnames=list(c("man", "woman"), c("bicycle", "walk")))

#       bicycle walk
# man         2   15
# woman      10    3

# Chi-squared test
chisq.test(d)
# X-squared = 10.4581, df = 1, p-value = 0.001221

# Fisher exact test
fisher.test(d)
# p-value = 0.0005367
