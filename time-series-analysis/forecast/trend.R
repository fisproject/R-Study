# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# convert char-code "nkf -w --overwrite filename"
df <- read.csv("data/7201.T.csv", header = T)
names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

df.zoo <- zoo::zoo(df$Close, order.by=as.Date(df$Date, format='%Y/%m/%d'))
# to create time-series object
df.ts <- ts(df.zoo, frequency = 245)

# decompose : data = seasonal + trend + residualError
stl <- stl(df.ts, "periodic")
plot(stl)
