library(vars)
library(quantmod)
library(ggplot2)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# 国債名目利回り : http://www.mof.go.jp/jgbs/reference/interest_rate/
d <- read.csv("data/jp-bond.csv", header = T)

bond <- data.frame(date = as.Date(d[,1]), ten_years = as.numeric(as.character(d[,11])))
index <- which(bond[, 1] > as.Date("1986/07/31"))
bond <- bond[index,, drop = F]
index <- which(bond[, 1] < as.Date("2015/07/01"))
bond <- bond[index,, drop = F]

bond.xts <- xts(bond[,2], order.by = as.POSIXct(bond$date)) # convert xts object
bond.monthly <- apply.monthly(bond.xts, colMeans) # means per month
bond.df <- data.frame(index = c(1:347), value = bond.monthly)

bond <- data.frame(date = as.Date(bond[,1]), ten_years = bond[,2])

g <- ggplot(bond, aes(x = date, y = ten_years)) +
  geom_line(colour = "magenta") + labs(title = "", x = "", y = "日本10年国債利回り (%)")
plot(g)

# 諸費者物価指数 : http://www.e-stat.go.jp/SG1/estat/List.do?bid=000001033702&cycode=0
dd <- read.csv("data/cpi.csv", header = T)

x <- c()
# インフレ率 (%) = (その期の消費者物価指数 - 前年同月の消費者物価指数) / 前年同月の消費者物価指数　* 100
for (i in 12:546) {
  x[i] <- (dd[i,2] - dd[i-12,2]) / dd[i-12,2] * 100
}

inflation <- data.frame(date = as.Date(dd[,1]), cpi = dd[,2], rate = x)

g <- ggplot(inflation, aes(x = date, y = rate)) +
  geom_line(colour = "magenta") +
  labs(title = "", x = "", y = "インフレ率 (%)")
plot(g)

index <- which(inflation[, 1] > as.Date("1986/07/31"))
inflation <- inflation[index,, drop = F]
index <- which(inflation[, 1] < as.Date("2015/07/01"))
inflation <- inflation[index,, drop = F]

inflation.xts <- xts(inflation[,3], order.by = as.POSIXct(inflation$date)) # convert xts object
inflation.monthly <- apply.monthly(inflation.xts, colMeans) # means per month
inflation.df <- data.frame(index = c(1:347), value = inflation.monthly)

# 日経平均株価 終値
ddd <- read.csv("data/nikkei-ave.csv", header = T)
stock <- data.frame(date = as.Date(ddd[,1]), close = as.numeric(as.character(ddd[,5])))
index <- which(stock[, 1] > as.Date("1986/07/31"))
stock <- stock[index,, drop = F]
index <- which(stock[, 1] < as.Date("2015/07/01"))
stock <- stock[index,, drop = F]

g <- ggplot(NULL) + geom_line(data = stock, aes(x = date, y = close))
plot(g)

# 実質金利　=　名目金利　-　インフレ率
interest.df <- data.frame(date = stock[,1], value = bond.df[,2] - inflation.df[,2])
g <- ggplot(NULL) + geom_line(data = interest.df, aes(x = date, y = value))
plot(g)

# Granger causality
df <- data.frame(date = stock[,1], real_interest = bond.df[,2]-inflation.df[,2], stock = stock[,2])

g <- ggplot(NULL) + geom_line(data = df, aes(x = date, y = scale(real_interest)), colour = "cyan")
g <- g + geom_line(data = df, aes(x = date, y = scale(stock)), colour = "magenta") +
  labs(title = "", x = "", y = "実質金利, 日経平均株価 (標準化)")
plot(g)

df.all <- df[,c(2,3)] # remove date
VARselect(df.all)
df.var <- VAR(df.all, p = 3)

causality(df.var, cause = "real_interest")
# $Granger
#
# 	Granger causality H0: real_interest do not Granger-cause stock
#
# data:  VAR object df.var
# F-Test = 1.6362, df1 = 3, df2 = 674, p-value = 0.1797
#
#
# $Instant
#
# 	H0: No instantaneous causality between: real_interest and stock
#
# data:  VAR object df.var
# Chi-squared = 0.4931, df = 1, p-value = 0.4825

causality(df.var, cause = "stock")
# $Granger
#
# 	Granger causality H0: stock do not Granger-cause real_interest
#
# data:  VAR object df.var
# F-Test = 3.0766, df1 = 3, df2 = 674, p-value = 0.02706
#
#
# $Instant
#
# 	H0: No instantaneous causality between: stock and real_interest
#
# data:  VAR object df.var
# Chi-squared = 0.4931, df = 1, p-value = 0.4825


# Granger causality (to 2005/07/01)
index <- which(df[, 1] < as.Date("2005/07/01"))
df.part <- df[index,, drop = F]

g <- ggplot(NULL) + geom_line(data = df.part, aes(x = date, y = scale(real_interest)), colour = "cyan")
g <- g + geom_line(data = df.part, aes(x = date, y = scale(stock)), colour = "magenta") +
  labs(title = "", x = "", y = "実質金利, 日経平均株価 (標準化)")
plot(g)

df.part <- df.part[,c(2,3)] # remove date
VARselect(df.part)
df.var <- VAR(df.part, p = 4)
causality(df.var, cause = "real_interest")
# $Granger
#
# 	Granger causality H0: real_interest do not Granger-cause stock
#
# data:  VAR object df.var
# F-Test = 1.8883, df1 = 4, df2 = 428, p-value = 0.1115
#
#
# $Instant
#
# 	H0: No instantaneous causality between: real_interest and stock
#
# data:  VAR object df.var
# Chi-squared = 1.0115, df = 1, p-value = 0.3146
#

causality(df.var, cause = "stock")
# $Granger
#
# 	Granger causality H0: stock do not Granger-cause real_interest
#
# data:  VAR object df.var
# F-Test = 2.6614, df1 = 4, df2 = 428, p-value = 0.03225
#
#
# $Instant
#
# 	H0: No instantaneous causality between: stock and real_interest
#
# data:  VAR object df.var
# Chi-squared = 1.0115, df = 1, p-value = 0.3146


# Granger causality (from 2005/07/01)
index <- which(df[, 1] > as.Date("2005/07/01"))
df.part <- df[index,, drop=F]

g <- ggplot(NULL) + geom_line(data = df.part, aes(x = date, y = scale(real_interest)), colour = "cyan")
g <- g + geom_line(data = df.part, aes(x = date, y = scale(stock)), colour = "magenta") +
  labs(title = "", x = "", y = "実質金利, 日経平均株価 (標準化)")
plot(g)

df.part <- df.part[,c(2,3)] # remove date
VARselect(df.part)
df.var <- VAR(df.part, p = 2)
causality(df.var, cause = "real_interest")
# $Granger
#
# 	Granger causality H0: real_interest do not Granger-cause stock
#
# data:  VAR object df.var
# F-Test = 0.4364, df1 = 2, df2 = 226, p-value = 0.6469
#
#
# $Instant
#
# 	H0: No instantaneous causality between: real_interest and stock
#
# data:  VAR object df.var
# Chi-squared = 1.0054, df = 1, p-value = 0.316

causality(df.var, cause = "stock")
# $Granger
#
# 	Granger causality H0: stock do not Granger-cause real_interest
#
# data:  VAR object df.var
# F-Test = 0.5327, df1 = 2, df2 = 226, p-value = 0.5878
#
#
# $Instant
#
# 	H0: No instantaneous causality between: stock and real_interest
#
# data:  VAR object df.var
# Chi-squared = 1.0054, df = 1, p-value = 0.316
