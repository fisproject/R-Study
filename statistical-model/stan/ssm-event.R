library(dplyr)
library(reshape)
library(ggplot2)
library(rstan)
library(ggmcmc)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

rstan_options(auto_write = TRUE)
options(mc.cores=parallel::detectCores())

df <- read.csv("data/pv.csv")

stan_data <- list(
  N = nrow(df),
  week_day = df$week,
  advertisement = df$ad,
  vacation = df$vacation,
  y = df$pv
)

model.fit <- stan(
  file = 'model/ssm-event.stan',
  data = stan_data,
  iter = 3000,
  warmup = 500,
  chains = 3,
  seed = 123456,
  verbose = TRUE
)

# output summary to log
options(max.print = 100000)
con <- file("summary-ssm-event.log")
sink(con, append = TRUE)
print(model.fit, digits = 2)
sink()
options(max.print = 1000)


params <- ggs(model.fit)

# Week
param.week <- params %>% filter(grepl("^s", Parameter))
w <- c("Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed")

df.week <- data.frame(
  week = factor(w, levels = w),
  week.med = ci(param.week)$median[1:7]
)

g <- ggplot(df.week, aes(x = week, y = week.med, group = 1)) +
  geom_point(stat = 'summary', fun.y = sum) +
  stat_summary(fun.y = sum, geom = "line") +
  labs(title = "Seasonal Component", x = "Day of the week", y = "pv (median)")
g

# Trend and AR
param.trend <- params %>% filter(grepl("^trend", Parameter))
param.ar <- params %>% filter(grepl("^ar", Parameter))

df.trend <- data.frame(
  date = as.Date(df$date),
  trend.med = ci(param.trend)$median[1:365],
  ar.med = ci(param.ar)$median[1:365],
  pv = df$pv
)

res.rs <- melt(df.trend, id.vars = "date")
g <- ggplot(res.rs, aes(x = date, y = value, colour = variable)) +
  geom_line() +
  labs(title = "Trend and AR", x = "Date", y = "pv")
g

# Event
param.ad <- params %>% filter(grepl("^ad", Parameter))
param.va <- params %>% filter(grepl("^va", Parameter))

df.event <- data.frame(
  date = as.Date(df$date),
  ad.med = ci(param.ad)$median[1:365],
  vacation.med = ci(param.va)$median[1:365],
  pv = df$pv
)

res.rs <- melt(df.event, id.vars = "date")
g <- ggplot(res.rs, aes(x = date, y = value, colour = variable)) +
  geom_line() +
  labs(title = "Event", x = "Date", y = "pv")
g
