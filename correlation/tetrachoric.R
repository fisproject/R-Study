library(psych)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- read.csv("data/gender.csv")
#     bought male female
# 1   true  350     50
# 2  false  210    400

x <- cbind(d$male, d$female)

# tetrachoric correlation
tetrachoric(x, na.rm=TRUE)
# [1] 0.76
#
#  with tau of
# [1] -0.26  0.14
