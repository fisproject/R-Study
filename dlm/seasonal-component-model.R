require(dlm)
require(ggplot2)
require(reshape2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

theme_set(theme_gray(base_family = "HiraKakuProN-W3"))

df <- read.csv("./data/pv.csv")
df$date <- as.Date(df$date)
df$pv <- as.numeric(df$pv)

buildFunc <- function(parm) {
  dlmModPoly(
    order = 2, # Local Linear Trend model
    dV = exp(parm[1]), # variance of the observation noise
    dW = c(exp(parm[2]), 0)) + # diagonal elements of the variance matrix of the system noise
  dlmModSeas(frequency = 7, dV = parm[1], dW = c(1, rep(0, 7 - 2))) # seasonal component
}

parm <- log(c(var(df$pv), 0.0001))

dlm.model <- buildFunc(parm)

dlm.filtered <- dlmFilter(df$pv, dlm.model) # Kalman filter

dlm.smoothed <- dlmSmooth(dlm.filtered) # Smoothing

# filtered value, one-step-ahead forecast (except the first value)
res <- data.frame(x = df$date[-1], filtered = dlm.filtered$m[-1:-2,1], oneStepAhead = dlm.filtered$f[-1], y = df$pv[-1])
res.rs <- melt(res, id.vars = "x")

g <- ggplot(res.rs, aes(x = x, y = value, colour = variable)) +
  geom_line() +
  labs(title = "Seasonal Component model (Kalman filter)", x = "Date", y = "pv")
g

# seasonal component
g <- ggplot(df, aes(x = date, y = dlm.smoothed$s[-1,3])) +
  geom_line() +
  labs(title = "Seasonal Component", x = "Date", y = "")
g

# MAPE
mean(abs(res$y - res$oneStepAhead) / res$y) * 100
# [1] 25.18414
