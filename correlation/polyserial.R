library(polycor)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- read.csv("data/quality.csv")
#     id quality price
# 1   1       1   980
# 2   2       2  2980
# 3   3       3  2980
# 4   4       4  4980
# 5   5       3  4480
# 6   6       2  1980
# 7   7       3  1480
# 8   8       3  1850
# 9   9       1  1200
# 10 10       5  9800

polyserial(d$price, d$quality, std.err=TRUE)
# Polyserial Correlation, 2-step est. = 0.9015 (0.06796)
# Test of bivariate normality: Chisquare = 10.83, df = 14, p = 0.6991
