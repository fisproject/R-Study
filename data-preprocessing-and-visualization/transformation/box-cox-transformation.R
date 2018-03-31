library(car)
library(ggplot2)
library(ggfortify)

set.seed(12345)

theme_set(theme_gray(base_family="HiraMaruProN-W4"))

bcInverse <- function(x, lambda) {
    if (lambda == 0) {
        exp(x)
    } else {
        (lambda * x + 1)^(1 / lambda)
    }
}

# Box Cox Method, univariate
est <- powerTransform(cycles ~ len + amp + load, Wool)
summary(est)

# 1/2 : square root, 0 : log, -1 : inverse
lambda <- est$roundlam

# histogram
p1 <- ggplot(Wool, aes(x = cycles), y = ..density..) + labs(title = "before")
p2 <- ggplot(Wool, aes(x = bcPower(cycles, lambda)), y = ..density..) +
    labs(title = "box-cox-transformation")

p <- new('ggmultiplot', plots=list(p1, p2), ncol=2)
p[1:2] <- p[1:2] + geom_histogram(alpha = 0.8, position = "identity", size = 1)
p

# qqplot
p1 <- qplot(sample = cycles, data = Wool) + labs(title = "before")
p2 <- qplot(sample = bcPower(cycles, lambda), data = Wool) +
  labs(title = "box-cox-transformation")
p <-  new('ggmultiplot', plots=list(p1, p2), ncol=2)
p

model.lm <- lm(cycles ~ len + amp + load, Wool)
summary(model.lm)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 4521.370   1621.721   2.788 0.010454 *
# len           13.200      2.301   5.736 7.66e-06 ***
# amp         -535.833    115.057  -4.657 0.000109 ***
# load         -62.167     23.011  -2.702 0.012734 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 488.1 on 23 degrees of freedom
# Multiple R-squared:  0.7291,	Adjusted R-squared:  0.6937
# F-statistic: 20.63 on 3 and 23 DF,  p-value: 1.028e-06

# gamma GLM with a log link
model.glm <- glm(cycles ~ len + amp + load, family = Gamma(link = log), data = Wool)
summary(model.glm)
# Deviance Residuals:
#      Min        1Q    Median        3Q       Max
# -0.43387  -0.11573  -0.00931   0.10245   0.25346
#
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept) 10.4420740  0.5907520  17.676 7.01e-15 ***
# len          0.0168502  0.0008382  20.102 4.33e-16 ***
# amp         -0.6311986  0.0419124 -15.060 2.10e-13 ***
# load        -0.0770519  0.0083825  -9.192 3.66e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Gamma family taken to be 0.03161972)
#
#     Null deviance: 22.88393  on 26  degrees of freedom
# Residual deviance:  0.76935  on 23  degrees of freedom
# AIC: 332.76

# fit linear model with transformed response:
model.bc <- lm(bcPower(cycles, lambda) ~ len + amp + load, Wool)
summary(model.bc)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) 10.551813   0.616683  17.111 1.41e-14 ***
# len          0.016648   0.000875  19.025 1.43e-15 ***
# amp         -0.630866   0.043752 -14.419 5.22e-13 ***
# load        -0.078524   0.008750  -8.974 5.66e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.1856 on 23 degrees of freedom
# Multiple R-squared:  0.9658,	Adjusted R-squared:  0.9614
# F-statistic: 216.8 on 3 and 23 DF,  p-value: < 2.2e-16

df <- data.frame(
    predict.lm = predict(model.lm, Wool),
    predict.glm = exp(predict(model.glm, Wool)),
    predict.bc = bcInverse(predict(model.bc, Wool), lambda),
    actual = Wool$cycles
)

p1 <- ggplot(df, aes(x = predict.lm, y = actual)) +
    labs(title = "lm", x = "cycles-prediction-value", y = "cycles-actual-value")
p2 <- ggplot(df, aes(x = predict.glm, y = actual)) +
    labs(title = "glm", x = "cycles-prediction-value", y = "cycles-actual-value")
p3 <- ggplot(df, aes(x = predict.bc, y = actual)) +
    labs(title = "lm (box-cox-transformation)", x = "cycles-prediction-value", y = "cycles-actual-value")

p <- new('ggmultiplot', plots = list(p1, p2, p3), ncol=3)
p[1:3] <- p[1:3] + geom_point() + stat_smooth(method = 'lm')
p
