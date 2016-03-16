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

# Spearman's rank correlation rho
cor.test(d$satisfaction, d$price, method="s")
# data:  d$satisfaction and d$price
# S = 48.774, p-value = 0.02295
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#     rho
# 0.7044025
