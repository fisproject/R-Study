require(forecast)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# convert char-code "nkf -w --overwrite filename"
d <- read.csv("data/7201.T.csv", header = T)

# 5 years
rows <- 245 * 5
# convert header "Date","Open","High","Low","Close","Volume"
data <- data.frame(
    Date = d$日付[1:rows],
    Open = d$始値[1:rows],
    High = d$高値[1:rows],
    Low = d$安値[1:rows],
    Close = d$終値[1:rows],
    Volume = d$出来高[1:rows],
    Adjusted = d$調整後終値[1:rows]
)

data.zoo <- zoo(data$Close, order.by = as.Date(data$Date, format = '%Y/%m/%d'))
# time series
data.ts <- ts(data.zoo)

# Plots a time series along with its acf (AutoCorrelation Function) and either its pacf (Partial ACF),
# lagged scatterplot or spectrum.
par(family = "HiraKakuProN-W3")
tsdisplay(data.ts, main = "日産")

model <- auto.arima(
    data.ts,
    max.p = 2, # AR degree
    max.q = 2, # MA degree
    ic = "aic",# model selection.
    trace = T, # reported option
    stepwise = T
)
# ARIMA(2,1,2) with drift         : 10281.96
# ARIMA(0,1,0) with drift         : 10279.18
# ARIMA(1,1,0) with drift         : 10279.32
# ARIMA(0,1,1) with drift         : 10280.52
# ARIMA(0,1,0)                    : 10278.63
# ARIMA(1,1,1) with drift         : 10281.26
#
# Best model: ARIMA(0,1,0) with drift

summary(model)
# sigma^2 estimated as 260.4:  log likelihood=-5136.17
# AIC=10276.34   AICc=10276.35   BIC=10286.56
#
# Training set error measures:
#                        ME     RMSE      MAE         MPE     MAPE     MASE
# Training set 0.0005219955 16.12392 12.06049 -0.02388146 1.412625 1.000159
#                    ACF1
# Training set 0.02431441
# Training set 0.09790725

f <- forecast(
    model,
    level = c(50, 95), # confidence interval
    h = 20*3 # after 3 months
)
plot(f)
