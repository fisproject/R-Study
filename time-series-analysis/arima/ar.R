library(dplyr)
library(tseries)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# convert char-code "nkf -w --overwrite filename"
df <- read.csv("data/7201.T.csv", header = T)
names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
df$Date <- df$Date %>% as.Date()

df <- df %>% filter(Date > as.Date('2010/01/01'))
df.zoo <- zoo::zoo(df$Close, order.by = df$Date)
close <- ts(df.zoo) # to time-series

adf.test(close)
# Augmented Dickey-Fuller Test
#
# data:  close
# Dickey-Fuller = -2.6838, Lag order = 10, p-value = 0.2889
# alternative hypothesis: stationary

PP.test(close)
# Phillips-Perron Unit Root Test
#
# data:  close
# Dickey-Fuller = -2.8926, Truncation lag parameter = 7, p-value = 0.2005

close.diff <- diff(log(close))

adf.test(close.diff)
# Augmented Dickey-Fuller Test
#
# data:  close.diff
# Dickey-Fuller = -11.138, Lag order = 10, p-value = 0.01
# alternative hypothesis: stationary

PP.test(close.diff)
# Phillips-Perron Unit Root Test
#
# data:  close.diff
# Dickey-Fuller = -35.791, Truncation lag parameter = 7, p-value = 0.01

# AR model
model <- ar(close.diff, aic = TRUE, method = "ols")

model$order
 #[1] 0

model$x.mean
# [1] 0.0003618929

plot(model$resid)

# Eval
tseries::jarque.bera.test(model$resid[7:200])
# Jarque Bera Test
#
# data:  model$resid[7:200]
# X-squared = 0.90988, df = 2, p-value = 0.6345

Box.test(model$resid[7:200], lag = 30)
# Box-Pierce test
#
# data:  model$resid[7:200]
# X-squared = 23.125, df = 30, p-value = 0.8101

# Prediction
prediction <- predict(model, tail(close.diff, n = 1), n.ahead = 1)
# $pred
# Time Series:
# Start = 2 
# End = 2 
# Frequency = 1 
# [1] 0.0003618929

# $se
# Time Series:
# Start = 2 
# End = 2 
# Frequency = 1 
# [1] 0.01885173
