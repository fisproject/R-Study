library(tseries)
library(urca)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

df <- read.csv("data/pv.csv", header = T)

# 1. Engle-Granger test
model <- lm(pv ~ ad + vacation, data = df)

summary(model)
#
# Call:
# lm(formula = pv ~ ad + vacation, data = df)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -401.23 -184.76   44.24  120.24  612.24
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  446.755      9.188  48.622  < 2e-16 ***
# ad            14.837      1.840   8.065 1.09e-14 ***
# vacation    -199.612     38.044  -5.247 2.64e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 169.2 on 362 degrees of freedom
# Multiple R-squared:  0.2081,	Adjusted R-squared:  0.2037
# F-statistic: 47.56 on 2 and 362 DF,  p-value: < 2.2e-16

model.resid <- resid(model)

plot(model.resid, type = "l")

adf.test(model.resid)
#
# 	Augmented Dickey-Fuller Test
#
# data:  model.resid
# Dickey-Fuller = -3.9109, Lag order = 7, p-value = 0.01373
# alternative hypothesis: stationary


# 2. Phillips-Ouliaris test
x <- data.frame(df$pv, df$ad, df$vacation)

pu <- ca.po(x, demean = "const", type = "Pu")

summary(pu)
# ########################################
# # Phillips and Ouliaris Unit Root Test #
# ########################################
#
# Test of type Pu
# detrending of series with constant only
#
#
# Call:
# lm(formula = z[, 1] ~ z[, -1])
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -401.23 -184.76   44.24  120.24  612.24
#
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)
# (Intercept)         446.755      9.188  48.622  < 2e-16 ***
# z[, -1]df.ad         14.837      1.840   8.065 1.09e-14 ***
# z[, -1]df.vacation -199.612     38.044  -5.247 2.64e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 169.2 on 362 degrees of freedom
# Multiple R-squared:  0.2081,	Adjusted R-squared:  0.2037
# F-statistic: 47.56 on 2 and 362 DF,  p-value: < 2.2e-16
#
#
# Value of test-statistic is: 137.5592
#
# Critical values of Pu are:
#                   10pct    5pct    1pct
# critical values 33.6955 40.5252 53.8731
