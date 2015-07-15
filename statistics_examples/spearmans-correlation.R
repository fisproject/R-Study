# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

data <- read.csv("data/satisfaction.csv")
# id satisfaction price gender
# 1   1            5     4       1
# 2   2            2     1       0
# 3   3            3     2       0
# 4   4            1     2       0
# 5   5            1     0       1
# 6   6            2     3       0
# 7   7            3     1       1
# 8   8            3     3       0
# 9   9            4     5       1
# 10 10            0     1       0

# Spearman's rank correlation rho
cor.test(data$satisfaction, data$price, method="s")
# rho
# 0.7044025
cor.test(data$satisfaction, data$gender, method="s")
# rho
# 0.4705699
cor.test(data$price, data$gender, method="s")
# rho
# 0.07239537
