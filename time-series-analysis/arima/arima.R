library(dplyr)
library(forecast)

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
close.diff <- diff(log(close))

# ARMA
model.arma = arima(close.diff, order = c(1, 0, 1))
summary(model.arma)
# Call:
# arima(x = close.diff, order = c(1, 0, 1))
#
# Coefficients:
#          ar1      ma1  intercept
#       0.1374  -0.1192      4e-04
# s.e.  0.6576   0.6564      5e-04
#
# sigma^2 estimated as 0.0003553:  log likelihood = 3389.56,  aic = -6771.12
#
# Training set error measures:
#                         ME       RMSE        MAE MPE MAPE      MASE         ACF1
# Training set -1.175445e-05 0.01884854 0.01422471 NaN  Inf 0.7092202 -0.001017524

head(model.arma$residuals, 20)

plot(model.arma$residuals)

# ARIMA
model.arima <- auto.arima(
    close,
    max.p = 2, # AR degree
    max.q = 2, # MA degree
    ic = "aic",# model selection.
    trace = T, # reported option
    stepwise = T
)

summary(model.arima)
# Series: close
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
# Training set error measures:
#                        ME     RMSE     MAE        MPE     MAPE     MASE
# Training set 0.0006137099 16.01399 12.0131 -0.0266852 1.422703 1.000106
#                    ACF1
# Training set 0.02340887
