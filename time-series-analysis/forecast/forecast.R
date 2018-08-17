library(forecast)
library(dplyr)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# convert char-code "nkf -w --overwrite filename"
df <- read.csv("data/7201.T.csv", header = T)
names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

df <- df %>%
  filter(as.Date('2010-01-01') < as.Date(.$Date))

df.zoo <- zoo::zoo(df$Close, order.by = as.Date(df$Date, format = '%Y/%m/%d'))
# to create time-series object
df.ts <- ts(df.zoo)

# Plots a time series along with its acf (AutoCorrelation Function) and either its pacf (Partial ACF),
# lagged scatterplot or spectrum.
par(family = "HiraKakuProN-W3")
tsdisplay(df.ts, main = "日産自動車")

model <- auto.arima(
    df.ts,
    max.p = 2, # AR degree
    max.q = 2, # MA degree
    ic = "aic",# model selection.
    trace = T, # reported option
    stepwise = T
)

summary(model)
# ARIMA(2,1,2) with drift         : 11139.28
# ARIMA(0,1,0) with drift         : 11134.47
# ARIMA(1,1,0) with drift         : 11136.04
# ARIMA(0,1,1) with drift         : 11135.78
# ARIMA(0,1,0)                    : 11133.21
# ARIMA(1,1,1) with drift         : 11137.57
#
# Best model: ARIMA(0,1,0) with drift

model.forecast <- forecast(
    model,
    level = c(50, 95), # confidence interval
    h = 20 * 3 # after 3 months
)

summary(model.forecast)
# Forecast method: ARIMA(0,1,0) with drift
#
# Model Information:
# Series: df.ts
# ARIMA(0,1,0) with drift
#
# Coefficients:
#        drift
#       0.3791
# s.e.  0.4396
#
# sigma^2 estimated as 256.8:  log likelihood=-5568.01
# AIC=11140.01   AICc=11140.02   BIC=11150.4
#
# Error measures:
#                        ME     RMSE     MAE        MPE     MAPE     MASE
# Training set 0.0006137099 16.01399 12.0131 -0.0266852 1.422703 1.000106
#                    ACF1
# Training set 0.02340887
#
# Forecasts:
#      Point Forecast    Lo 50    Hi 50    Lo 95    Hi 95
# 1330       1319.879 1309.070 1330.689 1288.469 1351.290
# 1331       1320.258 1304.971 1335.545 1275.837 1364.679
# 1332       1320.637 1301.915 1339.360 1266.233 1375.042
# 1333       1321.017 1299.398 1342.635 1258.196 1383.838

plot(model.forecast)