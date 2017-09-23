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

# plot pv
g <- ggplot(df, aes(x = date, y = pv)) +
  geom_line(colour = "blue", alpha = 0.5) +
  labs(title = "pv", x = "Date", y = "pv")
g

buildFunc <- function(parm) {
  dlmModPoly(
    order = 1, # Local Level model (Random Walk plus noise model)
    dV = exp(parm[1]), # variance of the observation noise
    dW = exp(parm[2]) # diagonal elements of the variance matrix of the system noise
  )
}

dlm.mle <- dlmMLE(
  y = df$pv,
  parm = c(0.0001, 0.0001), # vector of initial values
  build = buildFunc,
  method = "L-BFGS-B"
)

dlm.mle$convergence
# [1] 0

dlm.mle$par
# [1] 10.317543  6.021586

dlm.model <- buildFunc(dlm.mle$par)

dlm.filtered <- dlmFilter(df$pv, dlm.model) # Kalman filter

dlm.smoothed <- dlmSmooth(dlm.filtered) # Smoothing

# filtered value, one-step-ahead forecast (except the first value)
res <- data.frame(x = df$date[-1], filtered = dlm.filtered$m[-1:-2], oneStepAhead = dlm.filtered$f[-1], y = df$pv[-1])
res.rs <- melt(res, id.vars = "x")

g <- ggplot(res.rs, aes(x = x, y = value, colour = variable)) +
  geom_line() +
  labs(title = "Local Level model (Kalman filter)", x = "Date", y = "pv")
g

# MAPE
mean(abs(res$y - res$oneStepAhead) / res$y) * 100
# [1] 48.99133
