# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# convert char-code "nkf -w --overwrite filename"
df <- read.csv("data/7201.T.csv", header = T)

names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

df <- df %>% filter(as.Date(df$Date) > as.Date('2010/01/01'))
df.zoo <- zoo::zoo(df$Close, order.by = as.Date(df$Date, format = '%Y/%m/%d'))
# to time-series
close <- ts(df.zoo)

head(close)

tseries::adf.test(close)
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

tseries::adf.test(close.diff)
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
model.ar = ar(close.diff, aic = TRUE, model = "ols")

model.ar$order
 #[1] 0

model.ar$x.mean
# [1] 0.0003618929

head(model.ar$resid, 20)
# [1] -0.016421534 -0.005355661 -0.011689893  0.020928830  0.019270639 -0.028706366
# [7]  0.003381093 -0.002855660 -0.026928920 -0.015866079 -0.013468240  0.008830555
# [13] -0.020164520 -0.012434474 -0.019436041 -0.021210459  0.027340710  0.005087712
# [19] -0.020952874  0.033724570

plot(model.ar$resid)

# Eval
tseries::jarque.bera.test(model.ar$resid[7:200])
# Jarque Bera Test
#
# data:  model.ar$resid[7:200]
# X-squared = 0.90988, df = 2, p-value = 0.6345

Box.test(model.ar$resid[7:200], lag = 30)
# Box-Pierce test
#
# data:  model.ar$resid[7:200]
# X-squared = 23.125, df = 30, p-value = 0.8101
