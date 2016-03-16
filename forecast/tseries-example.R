require(tseries)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# convert char-code "nkf -w --overwrite filename"
d <- read.csv("data/7201.T.csv", header=T)

# 5 years
raw <- 245*5
# convert header "Date","Open","High","Low","Close","Volume"
data <- data.frame(
          Date=d$日付[1:raw],
          Open=d$始値[1:raw],
          High=d$高値[1:raw],
          Low=d$安値[1:raw],
          Close=d$終値[1:raw],
          Volume=d$出来高[1:raw],
          Adjusted=d$調整後終値[1:raw]
        )

data.zoo <- zoo(data$Close, order.by=as.Date(data$Date, format='%Y/%m/%d'))
# time series
data.ts <- ts(data.zoo)

adf.test(data.ts)
# Augmented Dickey-Fuller Test
#
# data:  data.ts
# Dickey-Fuller = -2.506, Lag order = 10, p-value = 0.3641
# alternative hypothesis: stationary

adf.test(diff(data.ts))
# Augmented Dickey-Fuller Test
#
# data:  diff(data.ts)
# Dickey-Fuller = -10.6537, Lag order = 10, p-value = 0.01
# alternative hypothesis: stationary
