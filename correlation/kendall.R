library(Kendall)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- read.csv("data/satisfaction.csv")
#     id satisfaction price gender
# 1   1            5     4   male
# 2   2            2     1 female
# 3   3            3     2 female
# 4   4            1     2 female
# 5   5            1     0   male
# 6   6            2     3 female
# 7   7            3     1   male
# 8   8            3     3 female
# 9   9            4     5   male
# 10 10            0     1 female

# tau Kendall’s tau statistic
Kendall(d$satisfaction, d$price)
# tau = 0.575, 2-sided pvalue =0.039549

# or cor.test()
cor.test(d$satisfaction, d$price, method="k")
# Kendall's rank correlation tau
#
# data:  d$satisfaction and d$price
# z = 2.152, p-value = 0.0314
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
# tau
# 0.575
