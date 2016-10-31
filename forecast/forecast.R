require(forecast)
require(dplyr)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# convert char-code "nkf -w --overwrite filename"
df <- read.csv("data/7201.T.csv", header = T)
names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

df <- df %>% filter(as.Date(df$Date) > as.Date('2010/01/01'))
df.zoo <- zoo(df$Close, order.by = as.Date(df$Date, format = '%Y/%m/%d'))
# to time series
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

f <- forecast(
    model,
    level = c(50, 95), # confidence interval
    h = 20 * 3 # after 3 months
)
plot(f)
