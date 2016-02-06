require(dplyr)

set.seed(12345)

norm <- rnorm(20)
shapiro.test(norm)
# Shapiro-Wilk normality test
#
# data:  norm
# W = 0.97869, p-value = 0.916

d <- c(0, 0, 20, 15, 9, 40, 0, 0, 0, 18, 0, 23, 8, 0, 0, 12, 0, 7, 0, 17)
shapiro.test(d)
# Shapiro-Wilk normality test
#
# data:  d
# W = 0.78822, p-value = 0.0005795

s <- sleep
head(s)
# extra group ID
# 1   0.7     1  1
# 2  -1.6     1  2
# 3  -0.2     1  3
# 4  -1.2     1  4
# 5  -0.1     1  5
# 6   3.4     1  6

grp1 <- s %>% filter(group == 1)
grp2 <- s %>% filter(group == 2)

shapiro.test(grp1$extra)
# Shapiro-Wilk normality test
#
# data:  grp1$extra
# W = 0.92581, p-value = 0.4079

shapiro.test(grp2$extra)
# Shapiro-Wilk normality test
#
# data:  grp2$extra
# W = 0.9193, p-value = 0.3511
