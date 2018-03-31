library(forecast)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

df <- read.csv("data/pv.csv", header = T)

best <- auto.arima(
  ts(df$pv),
  max.p = 7, # AR degree
  max.q = 7, # MA degree
  ic = "aic",# model selection.
  trace = T, # reported option
  stepwise = T
)

summary(best)
# Series: df.ts
# ARIMA(0,0,3) with non-zero mean
#
# Coefficients:
#          ma1     ma2      ma3      mean
#       0.7789  0.1217  -0.1181  443.7071
# s.e.  0.0529  0.0683   0.0595   13.7238
#
# sigma^2 estimated as 21921:  log likelihood=-2340.39
# AIC=4690.77   AICc=4690.94   BIC=4710.27
#
# Training set error measures:
#                      ME     RMSE      MAE       MPE     MAPE      MASE
# Training set 0.03101651 147.2451 111.2352 -15.06934 33.71827 0.8765312
#                     ACF1
# Training set 0.008160298

model <- forecast(
  best,
  level = c(50, 95), # confidence interval
  h = 7 * 4
)

res <- model$res[-1]

k <- trunc((length(res)-1)^(1/3))
Box.test(res, lag = k, type = "Ljung-Box", fitdf = 0)
#
# 	Box-Ljung test
#
# data:  res
# X-squared = 105.57, df = 7, p-value < 2.2e-16

set.seed(123)
x <- rnorm (1000)
k <- trunc((length(x)-1)^(1/3))
Box.test (x, lag = k, type = "Ljung-Box")
#
# 	Box-Ljung test
#
# data:  x
# X-squared = 4.7818, df = 9, p-value = 0.8529
