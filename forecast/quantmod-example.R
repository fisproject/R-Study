# https://github.com/joshuaulrich/quantmod
require(quantmod)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# convert char-code "nkf -w --overwrite filename"
d <- read.csv("data/7201.T.csv", header = T)

# 2 years
rows <- 245*2
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

# head(data)
#       Date   Open   High    Low  Close   Volume  Adjusted
# 1  2015/6/3 1322.0 1350.0 1319.5 1319.5 16815000   1319.5
# 2  2015/6/2 1313.0 1326.5 1300.5 1326.0 14139600   1326.0
# 3  2015/6/1 1293.0 1304.0 1282.5 1302.5  8270000   1302.5
# 4 2015/5/29 1300.0 1308.5 1289.5 1300.5 10061300   1300.5
# 5 2015/5/28 1298.5 1312.0 1293.0 1299.0 14069200   1299.0
# 6 2015/5/27 1270.0 1291.0 1262.0 1280.5 12494400   1280.5

ohlc <- read.zoo(data, header = T)

par(family = "HiraKakuProN-W3")
candleChart(ohlc, name = "日産", multi.col = TRUE, theme = "white")
