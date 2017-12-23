# http://www.stat.sc.edu/~hitchcock/stat599chap7Rcode.txt
library(faraway)
library(ggplot2)
library(ggfortify)

# 半導体ウェハの抵抗値測定 resitivity of wafer in semiconductor experiment
data(wafer)
attach(wafer)

wafer.lm <- lm(log(resist) ~ (x1+x2+x3+x4)^2, data = wafer)

step.lognorm.wafer <- step(wafer.lm)
summary(step.lognorm.wafer)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  5.31111    0.04762 111.525 4.67e-14 ***
# x1+          0.20088    0.04762   4.218  0.00292 **
# x2+         -0.21073    0.04762  -4.425  0.00221 **
# x3+          0.43718    0.06735   6.491  0.00019 ***
# x4+          0.03537    0.04762   0.743  0.47892
# x1+:x3+     -0.15621    0.06735  -2.319  0.04896 *
# x2+:x3+     -0.17824    0.06735  -2.647  0.02941 *
# x3+:x4+     -0.18303    0.06735  -2.718  0.02635 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.06735 on 8 degrees of freedom
# Multiple R-squared:  0.9471,	Adjusted R-squared:  0.9008
# F-statistic: 20.46 on 7 and 8 DF,  p-value: 0.000165

# Fitting the same model but with a gamma GLM with a log link:
wafer.glm <- glm(resist ~ (x1+x2+x3+x4)^2, family = Gamma(link = log), data = wafer)

# "link=log" must be specified, since the canonical link
step.gamma.wafer <- step(wafer.glm)
summary(step.gamma.wafer)
# Deviance Residuals:
#       Min         1Q     Median         3Q        Max
# -0.083185  -0.036793  -0.000648   0.038199   0.081390
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  5.31195    0.04757 111.677 4.62e-14 ***
# x1+          0.20029    0.04757   4.211  0.00295 **
# x2+         -0.21101    0.04757  -4.436  0.00218 **
# x3+          0.43674    0.06727   6.493  0.00019 ***
# x4+          0.03537    0.04757   0.744  0.47836
# x1+:x3+     -0.15549    0.06727  -2.312  0.04957 *
# x2+:x3+     -0.17626    0.06727  -2.620  0.03064 *
# x3+:x4+     -0.18195    0.06727  -2.705  0.02687 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.004524942)
#
#     Null deviance: 0.697837  on 15  degrees of freedom
# Residual deviance: 0.036266  on  8  degrees of freedom
# AIC: 139.2
#
# Number of Fisher Scoring iterations: 4

df <- data.frame(
    actual = resist,
    predict.glm = step.gamma.wafer$fitted.values,
    predict.lm = step.lognorm.wafer$fitted.values
)

p1 <- ggplot(df, aes(x = exp(predict.lm), y = actual)) + labs(title = "lm", x = "predict", y = "actual")
p2 <- ggplot(df, aes(x = predict.glm, y = actual)) + labs(title = "glm", x = "predict", y = "actual")

p <- new('ggmultiplot', plots = list(p1, p2), ncol=2)
p[1:2] <- p[1:2] + geom_point()
p
