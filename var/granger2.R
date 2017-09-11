require(vars)
require(quantmod)
require(ggplot2)
require(reshape2)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# 日本国債利回り : http://www.mof.go.jp/jgbs/reference/interest_rate/
d <- read.csv("data/jp-bond.csv", header = T)

bond <- data.frame(date = as.Date(d[,1]), two_years = as.numeric(as.character(d[,3])))
index <- which(bond[, 1] > as.Date("2000/06/30"))
bond <- bond[index,, drop = F]
index <- which(bond[, 1] < as.Date("2015/08/01"))
bond <- bond[index,, drop = F]

bond.xts <- xts(bond[,2], order.by = as.POSIXct(bond$date)) # convert xts object
bond.monthly <- apply.monthly(bond.xts, colMeans) # means per month
bond.df <- data.frame(index = c(1:181), value = bond.monthly)

bond <- data.frame(date = as.Date(bond[,1]), two_years = bond[,2])

# 米国債利回り : http://www.federalreserve.gov/releases/h15/data.htm
dd <- read.csv("data/us-bond-2years.csv", header = T)
bond.us <- data.frame(date = as.Date(bond.df[,1]), date.us = dd[,1], two_years = as.numeric(as.character(dd[,2])))

# 日経平均株価　終値
ddd <- read.csv("data/nikkei-ave.csv", header = T)
stock <- data.frame(date = as.Date(ddd[,1]), close = as.numeric(as.character(ddd[,5])))
index <- which(stock[,1] > as.Date("2000/06/30"))
stock <- stock[index,, drop = F]
index <- which(stock[,1] < as.Date("2015/08/01"))
stock <- stock[index,, drop = F]

g <-  ggplot(NULL) + geom_line(data = stock, aes(x = date, y = close))
plot(g)

# 日米 2年国債利回り
g <- ggplot(NULL) + geom_line(data = bond.us, aes(x = stock[,1], y = two_years), colour = "cyan")
g <- g + geom_line(data = bond.df, aes(x = stock[,1], y = value), colour = "magenta") +
  labs(title = "", x = "", y = "日米 2年国債利回り (%)")
plot(g)

#日米 2年国債利回り 差分
bond.diff <- data.frame(date = as.Date(bond.df[,1]), value = bond.us[,3] - bond.df[,2])
g <- ggplot(NULL) + geom_line(data = bond.diff, aes(x = date, y = value)) +
  labs(title = "", x = "", y = "日米 2年国債利回りの差分 (%)")
plot(g)

# Granger causality
df <- data.frame(date = stock[,1], interest_diff = bond.diff[,2], stock = stock[,2])
g <- ggplot(NULL) + geom_line(data = df, aes(x = date, y = scale(interest_diff)), colour = "cyan")
g <- g + geom_line(data = df, aes(x = date, y = scale(stock)), colour = "magenta") +
  labs(title = "", x = "", y = "日米金利差, 日経平均株価 (標準化)")
plot(g)

df.all <- df[,c(2,3)] # remove date
VARselect(df.all)
df.var <- VAR(df.all, p = 3)

causality(df.var, cause = "interest_diff")
# $Granger
#
# 	Granger causality H0: interest_diff do not Granger-cause stock
#
# data:  VAR object df.var
# F-Test = 2.1702, df1 = 3, df2 = 342, p-value = 0.0913
#
#
# $Instant
#
# 	H0: No instantaneous causality between: interest_diff and stock
#
# data:  VAR object df.var
# Chi-squared = 10.4162, df = 1, p-value = 0.001249


causality(df.var, cause = "stock")
# $Granger
#
# 	Granger causality H0: stock do not Granger-cause interest_diff
#
# data:  VAR object df.var
# F-Test = 8.1504, df1 = 3, df2 = 342, p-value = 2.954e-05
#
#
# $Instant
#
# 	H0: No instantaneous causality between: stock and interest_diff
#
# data:  VAR object df.var
# Chi-squared = 10.4162, df = 1, p-value = 0.001249
