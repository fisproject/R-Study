library(polycor)

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

# rho the polychoric correlation.
polychor(d$satisfaction, d$price)
# [1] 0.7377534

polychor(d$satisfaction, d$price, ML=TRUE, std.err=TRUE)
# Polychoric Correlation, ML est. = 0.7384 (0.1673)
# Test of bivariate normality: Chisquare = 15.39, df = 24, p = 0.9089
#
#   Row Thresholds
#   Threshold Std.Err.
# 1  -1.25300   0.5145
# 2  -0.53780   0.4094
# 3  -0.02593   0.3898
# 4   0.92270   0.4775
# 5   1.43200   0.5435
#
#
#   Column Thresholds
#   Threshold Std.Err.
# 1   -1.2430   0.5176
# 2   -0.2821   0.3923
# 3    0.2130   0.4006
# 4    0.9278   0.4769
# 5    1.4350   0.5431
