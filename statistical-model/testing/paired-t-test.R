library(ggplot2)
library(dplyr)

d <- sleep
head(d)
# extra group ID
# 1   0.7     1  1
# 2  -1.6     1  2
# 3  -0.2     1  3
# 4  -1.2     1  4
# 5  -0.1     1  5
# 6   3.4     1  6

p <- ggplot(d, aes(x = ID, y = extra, group = group, colour = group))

p <- p + geom_line() + geom_point() +
    labs(title = "sleep", x = "ID", y = "extra") + scale_colour_hue()
plot(p)

grp1 <- d %>% filter(group == 1)
grp2 <- d %>% filter(group == 2)

t.test(grp1$extra, grp2$extra, paired = T)
# Paired t-test
#
# data:  grp1$extra and grp2$extra
# t = -4.0621, df = 9, p-value = 0.002833
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -2.4598858 -0.7001142
# sample estimates:
# mean of the differences
#               -1.58
