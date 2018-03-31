library(tseries)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

df <- read.csv("data/pv.csv", header = T)

adf.test(df$pv)
#
# 	Augmented Dickey-Fuller Test
#
# data:  df$pv
# Dickey-Fuller = -3.9709, Lag order = 7, p-value = 0.01073
# alternative hypothesis: stationary

adf.test(seq(1, 100))
#
# 	Augmented Dickey-Fuller Test
#
# data:  seq(1, 100)
# Dickey-Fuller = 1.7321, Lag order = 4, p-value = 0.99
# alternative hypothesis: stationary

pp.test(df$pv)
#
# 	Phillips-Perron Unit Root Test
#
# data:  df$pv
# Dickey-Fuller Z(alpha) = -125.84, Truncation lag parameter = 5,
# p-value = 0.01
# alternative hypothesis: stationary
