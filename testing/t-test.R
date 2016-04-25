x <- c(1,2,1,1,1,1,1,1,1,1,2,4,1,1)
y <- c(3,3,4,3,1,2,3,1,1,5,4,6)

t.test(x, y, paired = F)

# t = -3.205, df = 16.101, p-value = 0.005484
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -2.7289451 -0.5567692
# sample estimates:
# mean of x mean of y
#  1.357143  3.000000
