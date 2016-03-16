require(dplyr)
require(ggplot2)
require(ggfortify)
require(data.table)
require(xts)

theme_set(theme_gray(base_family="HiraMaruProN-W4"))

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

analyze_financial_data <- function(data, func, window, interval){

  if( class(data)[1] == "xts" ){
      data <- as.numeric(data)
  }

  start <- interval + 1
  end <- length(data)
  return_ratio <- rep(NA, 1)

  for(i in start:end){
     # 対数収益率
     return_ratio <- append(return_ratio, log(data[i] / data[i-interval]))
  }

  result <- rollapply(return_ratio, window+1, func)

  padding_size <- length(data) - length(result)
  for(i in 1:padding_size){
      result <- c(NA, result)
  }
  return(result)
}

raw <- data.frame(fread("data/7201.T.csv", header = T))
data <- data.frame(date = as.Date(raw$日付), value = raw$終値)
data <- data %>% arrange(date)
data.xts <- as.xts(read.zoo(data))

# 終値
p <- ggplot(data, aes(x = date, y = value)) + labs(title = "7201") + geom_line()
p

# 日次収益率
data$ex20_1 <- analyze_financial_data(data.xts, mean, 20, 1)
data$ex250_1 <- analyze_financial_data(data.xts, mean, 250, 1)
data$ex750_1 <- analyze_financial_data(data.xts, mean, 750, 1)

# 週次収益率
data$ex20_5 <- analyze_financial_data(data.xts, mean, 20, 5)
data$ex250_5 <- analyze_financial_data(data.xts, mean, 250, 5)
data$ex750_5 <- analyze_financial_data(data.xts, mean, 750, 5)

# 月次収益率
data$ex20_20 <- analyze_financial_data(data.xts, mean, 20, 20)
data$ex250_20 <- analyze_financial_data(data.xts, mean, 250, 20)
data$ex750_20 <- analyze_financial_data(data.xts, mean, 750, 20)

# 収益率 - Line
p1 <- ggplot(data, aes(x = date, y = ex20_1)) + labs(title="日次収益率 (観測期間20日)") +
    geom_hline(aes(yintercept=mean(data$ex20_1, na.rm=TRUE)), colour = "magenta")
p2 <- ggplot(data, aes(x = date, y = ex250_1)) + labs(title="日次収益率 (観測期間250日)") +
    geom_hline(aes(yintercept=mean(data$ex250_1, na.rm=TRUE)), colour = "magenta")
p3 <- ggplot(data, aes(x = date, y = ex750_1)) + labs(title="日次収益率 (観測期間750日)") +
    geom_hline(aes(yintercept=mean(data$ex750_1, na.rm=TRUE)), colour = "magenta")

p4 <- ggplot(data, aes(x = date, y = ex20_5)) + labs(title="週次収益率 (観測期間20日)") +
    geom_hline(aes(yintercept=mean(data$ex20_5, na.rm=TRUE)), colour = "magenta")
p5 <- ggplot(data, aes(x = date, y = ex250_5)) + labs(title="週次収益率 (観測期間250日)") +
    geom_hline(aes(yintercept=mean(data$ex250_5, na.rm=TRUE)), colour = "magenta")
p6 <- ggplot(data, aes(x = date, y = ex750_5)) + labs(title="週次収益率 (観測期間750日)") +
    geom_hline(aes(yintercept=mean(data$ex750_5, na.rm=TRUE)), colour = "magenta")

p7 <- ggplot(data, aes(x = date, y = ex20_20)) + labs(title="月次収益率 (観測期間20日)") +
    geom_hline(aes(yintercept=mean(data$ex20_20, na.rm=TRUE)), colour = "magenta")
p8 <- ggplot(data, aes(x = date, y = ex250_20)) + labs(title="月次収益率 (観測期間250日)") +
    geom_hline(aes(yintercept=mean(data$ex250_20, na.rm=TRUE)), colour = "magenta")
p9 <- ggplot(data, aes(x = date, y = ex750_20)) + labs(title="月次収益率 (観測期間750日)") +
    geom_hline(aes(yintercept=mean(data$ex750_20, na.rm=TRUE)), colour = "magenta")

p <- new('ggmultiplot', plots=list(p1, p2, p3, p4, p5, p6, p7, p8, p9), ncol=3)
p[1:9] <- p[1:9] + geom_line() + ylim(-0.3, 0.3)
p

# 収益率 - Histgram
p1 <- ggplot(data, aes(x = ex20_1)) + labs(title="日次収益率 (観測期間20日)") +
    geom_vline(aes(xintercept=mean(data$ex20_1, na.rm=TRUE)), colour = "magenta")
p2 <- ggplot(data, aes(x = ex250_1)) + labs(title="日次収益率 (観測期間250日)") +
    geom_vline(aes(xintercept=mean(data$ex250_1, na.rm=TRUE)), colour = "magenta")
p3 <- ggplot(data, aes(x = ex750_1)) + labs(title="日次収益率 (観測期間750日)") +
    geom_vline(aes(xintercept=mean(data$ex750_1, na.rm=TRUE)), colour = "magenta")

p4 <- ggplot(data, aes(x = ex20_5)) + labs(title="週次収益率 (観測期間20日)") +
    geom_vline(aes(xintercept=mean(data$ex20_5, na.rm=TRUE)), colour = "magenta")
p5 <- ggplot(data, aes(x = ex250_5)) + labs(title="週次収益率 (観測期間250日)") +
    geom_vline(aes(xintercept=mean(data$ex250_5, na.rm=TRUE)), colour = "magenta")
p6 <- ggplot(data, aes(x = ex750_5)) + labs(title="週次収益率 (観測期間750日)") +
    geom_vline(aes(xintercept=mean(data$ex750_5, na.rm=TRUE)), colour = "magenta")

p7 <- ggplot(data, aes(x = ex20_20)) + labs(title="月次収益率 (観測期間20日)") +
    geom_vline(aes(xintercept=mean(data$ex20_20, na.rm=TRUE)), colour = "magenta")
p8 <- ggplot(data, aes(x = ex250_20)) + labs(title="月次収益率 (観測期間250日)") +
    geom_vline(aes(xintercept=mean(data$ex250_20, na.rm=TRUE)), colour = "magenta")
p9 <- ggplot(data, aes(x = ex750_20)) + labs(title="月次収益率 (観測期間750日)") +
    geom_vline(aes(xintercept=mean(data$ex750_20, na.rm=TRUE)), colour = "magenta")

p <- new('ggmultiplot', plots=list(p1, p2, p3, p4, p5, p6, p7, p8, p9), ncol=3)
p[1:9] <- p[1:9] + geom_histogram(position = "identity") + xlim(-0.25, 0.25)
p


# 日次収益率のボラティリティ
data$hv20_1 <- analyze_financial_data(data.xts, sd, 20, 1) * sqrt(250)
data$hv250_1 <- analyze_financial_data(data.xts, sd, 250, 1) * sqrt(250)
data$hv750_1 <- analyze_financial_data(data.xts, sd, 750, 1) * sqrt(250)

# 週次収益率のボラティリティ
data$hv20_5 <- analyze_financial_data(data.xts, sd, 20, 5) * sqrt(52)
data$hv250_5 <- analyze_financial_data(data.xts, sd, 250, 5) * sqrt(52)
data$hv750_5 <- analyze_financial_data(data.xts, sd, 750, 5) * sqrt(52)

# 月次収益率のボラティリティ
data$hv20_20 <- analyze_financial_data(data.xts, sd, 20, 20) * sqrt(12)
data$hv250_20 <- analyze_financial_data(data.xts, sd, 250, 20) * sqrt(12)
data$hv750_20 <- analyze_financial_data(data.xts, sd, 750, 20) * sqrt(12)

# ボラティリティ - Line
p1 <- ggplot(data, aes(x = date, y = hv20_1)) + labs(title="日次収益率のボラティリティ (観測期間20日)") +
    geom_hline(aes(yintercept=mean(data$hv20_1, na.rm=TRUE)), colour = "magenta")
p2 <- ggplot(data, aes(x = date, y = hv250_1)) + labs(title="日次収益率のボラティリティ (観測期間250日)") +
    geom_hline(aes(yintercept=mean(data$hv250_1, na.rm=TRUE)), colour = "magenta")
p3 <- ggplot(data, aes(x = date, y = hv750_1)) + labs(title="日次収益率のボラティリティ (観測期間750日)") +
    geom_hline(aes(yintercept=mean(data$hv750_1, na.rm=TRUE)), colour = "magenta")

p4 <- ggplot(data, aes(x = date, y = hv20_5)) + labs(title="週次収益率のボラティリティ (観測期間20日)") +
    geom_hline(aes(yintercept=mean(data$hv20_5, na.rm=TRUE)), colour = "magenta")
p5 <- ggplot(data, aes(x = date, y = hv250_5)) + labs(title="週次収益率のボラティリティ (観測期間250日)") +
    geom_hline(aes(yintercept=mean(data$hv250_5, na.rm=TRUE)), colour = "magenta")
p6 <- ggplot(data, aes(x = date, y = hv750_5)) + labs(title="週次収益率のボラティリティ (観測期間750日)") +
    geom_hline(aes(yintercept=mean(data$hv750_5, na.rm=TRUE)), colour = "magenta")

p7 <- ggplot(data, aes(x = date, y = hv20_20)) + labs(title="月次収益率のボラティリティ (観測期間20日)") +
    geom_hline(aes(yintercept=mean(data$hv20_20, na.rm=TRUE)), colour = "magenta")
p8 <- ggplot(data, aes(x = date, y = hv250_20)) + labs(title="月次収益率のボラティリティ (観測期間250日)") +
    geom_hline(aes(yintercept=mean(data$hv250_20, na.rm=TRUE)), colour = "magenta")
p9 <- ggplot(data, aes(x = date, y = hv750_20)) + labs(title="月次収益率のボラティリティ (観測期間750日)") +
    geom_hline(aes(yintercept=mean(data$hv750_20, na.rm=TRUE)), colour = "magenta")

p <- new('ggmultiplot', plots=list(p1, p2, p3, p4, p5, p6, p7, p8, p9), ncol=3)
p[1:9] <- p[1:9] + geom_line() + ylim(0, 1.2)
p

# ボラティリティ - Histgram
p1 <- ggplot(data, aes(x = hv20_1)) + labs(title="日次収益率のボラティリティ (観測期間20日)") +
    geom_vline(aes(xintercept=mean(data$hv20_1, na.rm=TRUE)), colour = "magenta")
p2 <- ggplot(data, aes(x = hv250_1)) + labs(title="日次収益率のボラティリティ (観測期間250日)") +
    geom_vline(aes(xintercept=mean(data$hv250_1, na.rm=TRUE)), colour = "magenta")
p3 <- ggplot(data, aes(x = hv750_1)) + labs(title="日次収益率のボラティリティ (観測期間750日)") +
    geom_vline(aes(xintercept=mean(data$hv750_1, na.rm=TRUE)), colour = "magenta")

p4 <- ggplot(data, aes(x = hv20_5)) + labs(title="週次収益率のボラティリティ (観測期間20日)") +
    geom_vline(aes(xintercept=mean(data$hv20_5, na.rm=TRUE)), colour = "magenta")
p5 <- ggplot(data, aes(x = hv250_5)) + labs(title="週次収益率のボラティリティ (観測期間250日)") +
    geom_vline(aes(xintercept=mean(data$hv250_5, na.rm=TRUE)), colour = "magenta")
p6 <- ggplot(data, aes(x = hv750_5)) + labs(title="週次収益率のボラティリティ (観測期間750日)") +
    geom_vline(aes(xintercept=mean(data$hv750_5, na.rm=TRUE)), colour = "magenta")

p7 <- ggplot(data, aes(x = hv20_20)) + labs(title="月次収益率のボラティリティ (観測期間20日)") +
    geom_vline(aes(xintercept=mean(data$hv20_20, na.rm=TRUE)), colour = "magenta")
p8 <- ggplot(data, aes(x = hv250_20)) + labs(title="月次収益率のボラティリティ (観測期間250日)") +
    geom_vline(aes(xintercept=mean(data$hv250_20, na.rm=TRUE)), colour = "magenta")
p9 <- ggplot(data, aes(x = hv750_20)) + labs(title="月次収益率のボラティリティ (観測期間750日)") +
    geom_vline(aes(xintercept=mean(data$hv750_20, na.rm=TRUE)), colour = "magenta")

p <- new('ggmultiplot', plots=list(p1, p2, p3, p4, p5, p6, p7, p8, p9), ncol=3)
p[1:9] <- p[1:9] + geom_histogram(position = "identity") + xlim(0, 1.2)
p


colnames(data)
# [1] "date"      "value"     "ex20_1"    "ex250_1"   "ex750_1"   "ex20_5"    "ex250_5"  
# [8] "ex750_5"   "ex20_20"   "ex250_20"  "ex750_20"  "hv20_1"    "hv250_1"   "hv750_1"
# [15] "hv20_5"    "hv250_5"   "hv750_5"   "hv20_20"   "hv250_20"  "hv750_20"

# VaR (Value at Risk) - 分散共分散法 (デルタ法)
data$var20_1 <- apply(data, 1, function(x) qnorm(0.01, as.numeric(x[3]), as.numeric(x[12]) * 1/sqrt(250)))
data$var250_1 <- apply(data, 1, function(x) qnorm(0.01, as.numeric(x[4]), as.numeric(x[13]) * 1/sqrt(250)))
data$var750_1 <- apply(data, 1, function(x) qnorm(0.01, as.numeric(x[5]), as.numeric(x[14]) * 1/sqrt(250)))

data$var20_5 <- apply(data, 1, function(x) qnorm(0.01, as.numeric(x[6]), as.numeric(x[15]) * 1/sqrt(52)))
data$var250_5 <- apply(data, 1, function(x) qnorm(0.01, as.numeric(x[7]), as.numeric(x[16]) * 1/sqrt(52)))
data$var750_5 <- apply(data, 1, function(x) qnorm(0.01, as.numeric(x[8]), as.numeric(x[17]) * 1/sqrt(52)))

data$var20_20 <- apply(data, 1, function(x) qnorm(0.01, as.numeric(x[9]), as.numeric(x[18]) * 1/sqrt(12)))
data$var250_20 <- apply(data, 1, function(x) qnorm(0.01, as.numeric(x[10]), as.numeric(x[19]) * 1/sqrt(12)))
data$var750_20 <- apply(data, 1, function(x) qnorm(0.01, as.numeric(x[21]), as.numeric(x[20]) * 1/sqrt(12)))

# VaR - Histgram
p1 <- ggplot(data, aes(x = var20_1)) + labs(title="99%VaR (保有期間1日, 観測期間20日)") +
    geom_vline(aes(xintercept=mean(data$var20_1, na.rm=TRUE)), colour = "magenta")
p2 <- ggplot(data, aes(x = var250_1)) + labs(title="99%VaR (保有期間1日, 観測期間250日)") +
    geom_vline(aes(xintercept=mean(data$var250_1, na.rm=TRUE)), colour = "magenta")
p3 <- ggplot(data, aes(x = var750_1)) + labs(title="99%VaR (保有期間1日, 観測期間750日)") +
    geom_vline(aes(xintercept=mean(data$var750_1, na.rm=TRUE)), colour = "magenta")

p4 <- ggplot(data, aes(x = var20_5)) + labs(title="99%VaR (保有期間5日, 観測期間20日)") +
    geom_vline(aes(xintercept=mean(data$var20_5, na.rm=TRUE)), colour = "magenta")
p5 <- ggplot(data, aes(x = var250_5)) + labs(title="99%VaR (保有期間5日, 観測期間250日)") +
    geom_vline(aes(xintercept=mean(data$var250_5, na.rm=TRUE)), colour = "magenta")
p6 <- ggplot(data, aes(x = var750_5)) + labs(title="99%VaR (保有期間5日, 観測期間750日)") +
    geom_vline(aes(xintercept=mean(data$var750_5, na.rm=TRUE)), colour = "magenta")

p7 <- ggplot(data, aes(x = var20_20)) + labs(title="99%VaR (保有期間20日, 観測期間20日)") +
    geom_vline(aes(xintercept=mean(data$var20_20, na.rm=TRUE)), colour = "magenta")
p8 <- ggplot(data, aes(x = var250_20)) + labs(title="99%VaR (保有期間20日, 観測期間250日)") +
    geom_vline(aes(xintercept=mean(data$var250_20, na.rm=TRUE)), colour = "magenta")
p9 <- ggplot(data, aes(x = var750_20)) + labs(title="99%VaR (保有期間20日, 観測期間750日)") +
    geom_vline(aes(xintercept=mean(data$var750_20, na.rm=TRUE)), colour = "magenta")

p <- new('ggmultiplot', plots=list(p1, p2, p3, p4, p5, p6, p7, p8, p9), ncol=3)
p[1:9] <- p[1:9] + geom_histogram(position = "identity") + xlim(-0.75, 0)
p

# Var (Value at Risk) - 分散共分散法 100万投資の場合
mean(data$var750_1, na.rm = TRUE) * 1000000
mean(data$var750_5, na.rm = TRUE) * 1000000
mean(data$var750_20, na.rm = TRUE) * 1000000

# Var (Value at Risk) - ヒストリカル法 100万投資の場合
unname(quantile(data$ex750_1, probs = seq(0.01, 1, 0.33), na.rm = TRUE))[1] * 1000000
unname(quantile(data$ex750_5, probs = seq(0.01, 1, 0.33), na.rm = TRUE))[1] * 1000000
unname(quantile(data$ex750_20, probs = seq(0.01, 1, 0.33), na.rm = TRUE))[1] * 1000000
