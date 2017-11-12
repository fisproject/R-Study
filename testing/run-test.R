require(tseries)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

df <- read.csv("data/pv.csv", header = T)

d <- diff(df$pv)
x <- factor(sign(d[-which(d %in% 0)])) # binarize
runs.test(x)
#
# 	Runs Test
#
# data:  x
# Standard Normal = -1.2316, p-value = 0.2181
# alternative hypothesis: two.sided

set.seed(123)
rnd <- factor(sign(rnorm(1000, 0, 1)))  # randomness
runs.test(rnd)
#
# 	Runs Test
#
# data:  rnd
# Standard Normal = 0.88913, p-value = 0.3739
# alternative hypothesis: two.sided
