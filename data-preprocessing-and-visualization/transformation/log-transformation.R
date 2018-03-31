library(ggplot2)
library(ggfortify)
library(numDeriv)

set.seed(12345)

# Artificial data
N <- 200
x <- rnorm(N, mean = 3, sd = 1)
y <- exp(x + runif(N, min = 1, max = 3))
df <- data.frame(x, y)

p1 <- ggplot(df, aes(y)) + labs(title = "before")
p2 <- ggplot(df, aes(log(y))) + labs(title = "log-transformation")

p <- new('ggmultiplot', plots=list(p1, p2), ncol=2)
p[1:2] <- p[1:2] + geom_histogram(alpha = 0.8, position = "identity", size = 1)
p

p1 <- ggplot(df, aes(x, y)) + labs(title = "before")
p2 <- ggplot(df, aes(x, log(y))) + labs(title = "log-transformation")

p <- new('ggmultiplot', plots=list(p1, p2), ncol=2)
p[1:2] <- p[1:2] + geom_point() + stat_smooth(method = 'lm')
p

model <- lm(y ~ x, df)
summary(model)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -730.41      81.36  -8.978   <2e-16 ***
# x             345.03      24.50  14.080   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 368.6 on 198 degrees of freedom
# Multiple R-squared:  0.5003,	Adjusted R-squared:  0.4978
# F-statistic: 198.3 on 1 and 198 DF,  p-value: < 2.2e-16

model.log <- lm(log(y) ~ x, df)
summary(model.log)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  1.93570    0.12080   16.02   <2e-16 ***
# x            1.02196    0.03638   28.09   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.5473 on 198 degrees of freedom
# Multiple R-squared:  0.7994,	Adjusted R-squared:  0.7984
# F-statistic:   789 on 1 and 198 DF,  p-value: < 2.2e-16

# Adjusted AIC
adjustedAIC <- function(m, m2) {
    AIC(m, m2)$AIC + c(0, -2 * sum(log(grad(sqrt, x = y))))
}
cbind(AIC(model, model.log), adjustedAIC(model, model.log))
# df       AIC AIC.adjust
# model      3 2935.4431   2585.441
# model.log  3  330.4272   1360.424
