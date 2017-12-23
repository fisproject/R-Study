library(lsr)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- read.csv("data/conditions.csv")
#     choice condition1 condition2
# 1      a         30         35
# 2      b         20         30
# 3      c         50         35

x <- cbind(d$condition1, d$condition2)

cramersV(x)
# [1] 0.1586139
