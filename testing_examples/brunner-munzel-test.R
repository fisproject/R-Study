# install.packages("lawstat")
library(lawstat)

x <- c(1,2,1,1,1,1,1,1,1,1,2,4,1,1)
y <- c(3,3,4,3,1,2,3,1,1,5,4)

brunner.munzel.test(x, y)
# Brunner-Munzel Test Statistic = 3.1375, df = 17.683, p-value = 0.005786
# 95 percent confidence interval:
#  0.5952169 0.9827052
# sample estimates:
# P(X<Y)+.5*P(X=Y) 
#         0.788961 