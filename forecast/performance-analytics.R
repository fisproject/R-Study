# https://github.com/joshuaulrich/quantmod
require(quantmod)
require(PerformanceAnalytics)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# char-code convert "nkf -w --overwrite filename"
df <- read.csv("data/7201.T.csv", header = T)
names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# convert xts object
df.xts <- xts(df[,-1], order.by = as.POSIXct(df$Date))

# Indicator : Relative Strength Index
rsi <- RSI(df.xts[,1])
plot(rsi)

# Strategy
str <- ifelse(
  rsi >= 0.5, # threshold
  1, # buy
  -1  # sell
)
# Signal
sig <- Lag(str)

# Return
ret <- ROC(df.xts[,1]) * sig
ret <- ret['2014-01-01/2015-06-01']
eq <- cumprod(1 + ret)
plot(eq)

# Eval (backtest)
table.Drawdowns(ret, top = 10)
table.DownsideRisk(ret)
charts.PerformanceSummary(ret)
