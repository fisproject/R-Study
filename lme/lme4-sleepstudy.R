require(lme4)
require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

data <- data.frame(sleepstudy)

# random intercept and random coefficient
ric.model <- lmer(Reaction ~ Days + (Days | Subject), data = data)
summary(ric.model)
# Linear mixed model fit by REML ['lmerMod']
# Formula: Reaction ~ Days + (Days | Subject)
#    Data: data
#
# REML criterion at convergence: 1743.6
#
# Scaled residuals:
#     Min      1Q  Median      3Q     Max
# -3.9536 -0.4634  0.0231  0.4634  5.1793
#
# Random effects:
#  Groups   Name        Variance Std.Dev. Corr
#  Subject  (Intercept) 612.09   24.740
#           Days         35.07    5.922   0.07
#  Residual             654.94   25.592
# Number of obs: 180, groups:  Subject, 18
#
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  251.405      6.825   36.84
# Days          10.467      1.546    6.77
#
# Correlation of Fixed Effects:
#      (Intr)
# Days -0.138

fixef(ric.model)
# (Intercept)        Days
#   251.40510    10.46729

ranef(ric.model)
# $Subject
#     (Intercept)        Days
# 308   2.2585654   9.1989719
# 309 -40.3985770  -8.6197032
# 310 -38.9602459  -5.4488799
# 330  23.6904985  -4.8143313
# 331  22.2602027  -3.0698946
# 332   9.0395259  -0.2721707
# 333  16.8404312  -0.2236244
# 334  -7.2325792   1.0745761
# 335  -0.3336959 -10.7521591
# 337  34.8903509   8.6282839
# 349 -25.2101104   1.1734143
# 350 -13.0699567   6.6142050
# 351   4.5778352  -3.0152572
# 352  20.8635925   3.5360133
# 369   3.2754530   0.8722166
# 370 -25.6128694   4.8224646
# 371   0.8070397  -0.9881551
# 372  12.3145394   1.2840297

# Plot
g <- ggplot(data, aes(x = Days, y = Reaction, colour = Subject)) + geom_point() +
    labs(title = "sleepstudy", x = "Days", y = "Reaction") +
    geom_abline(intercept = fixef(ric.model)[1], slope = fixef(ric.model)[2], lwd = 0.5, col = "black") + # Fixed effects
    geom_abline(intercept = fixef(ric.model)[1] + ranef(ric.model)$Subject[2,1],
        slope = fixef(ric.model)[2] + ranef(ric.model)$Subject[2,2], lwd = 0.5, col = "blue", linetype = 2) + # Random effects
    geom_abline(intercept = fixef(ric.model)[1] + ranef(ric.model)$Subject[10,1],
        slope = fixef(ric.model)[2] + ranef(ric.model)$Subject[10,2], lwd = 0.5, col = "red", linetype = 2)
plot(g)

# Re-fitting
ric.model.ML <- update(ric.model, REML = FALSE)

# Null Model
null.model <- lmer(Reaction ~ 1 + (1 | Subject), data = data)
summary(null.model)
# Linear mixed model fit by REML ['lmerMod']
# Formula: Reaction ~ 1 + (1 | Subject)
#    Data: data
#
# REML criterion at convergence: 1904.3
#
# Scaled residuals:
#     Min      1Q  Median      3Q     Max
# -2.4983 -0.5501 -0.1476  0.5123  3.3446
#
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Subject  (Intercept) 1278     35.75
#  Residual             1959     44.26
# Number of obs: 180, groups:  Subject, 18
#
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)   298.51       9.05   32.98

fixef(null.model)
# (Intercept)
#    298.5079

ranef(null.model)
# $Subject
#     (Intercept)
# 308   37.829172
# 309  -72.209815
# 310  -58.536725
# 330    4.087221
# 331    9.476087
# 332    7.625658
# 333   15.305131
# 334   -2.779868
# 335  -42.001705
# 337   66.953478
# 349  -19.660706
# 350   13.089079
# 351   -7.292650
# 352   33.743024
# 369    6.526637
# 370   -5.901763
# 371   -3.055622
# 372   16.803368

# random intercept model
ri.model <- lmer(Reaction ~ Days + (1 | Subject), data = data)
summary(ri.model)
# Linear mixed model fit by REML ['lmerMod']
# Formula: Reaction ~ Days + (1 | Subject)
#    Data: data
#
# REML criterion at convergence: 1786.5
#
# Scaled residuals:
#     Min      1Q  Median      3Q     Max
# -3.2257 -0.5529  0.0109  0.5188  4.2506
#
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Subject  (Intercept) 1378.2   37.12
#  Residual              960.5   30.99
# Number of obs: 180, groups:  Subject, 18
#
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept) 251.4051     9.7467   25.79
# Days         10.4673     0.8042   13.02
#
# Correlation of Fixed Effects:
#      (Intr)
# Days -0.371

fixef(ri.model)
# (Intercept)        Days
#   251.40510    10.46729

ranef(ri.model)
# $Subject
#     (Intercept)        Days
# 308   1.5126961   9.3234893
# 309 -40.3738984  -8.5991691
# 310 -39.1810427  -5.3877905
# 330  24.5189055  -4.9686458
# 331  22.9144343  -3.1939349
# 332   9.2219741  -0.3084936
# 333  17.1561219  -0.2872074
# 334  -7.4517338   1.1159901
# 335   0.5787255 -10.9059664
# 337  34.7679291   8.6276161
# 349 -25.7543247   1.2806878
# 350 -13.8650358   6.7564006
# 351   4.9159804  -3.0751330
# 352  20.9290432   3.5122096
# 369   3.2586475   0.8730507
# 370 -26.4758277   4.9837865
# 371   0.9056476  -1.0052929
# 372  12.4217579   1.2584028

# AIC, BIC, Likelihood ratio tests
anova(null.model, ric.model, ri.model)
# refitting model(s) with ML (instead of REML)
# Data: data
# Models:
# null.model: Reaction ~ 1 + (1 | Subject)
# ri.model: Reaction ~ Days + (1 | Subject)
# ric.model: Reaction ~ Days + (Days | Subject)
#            Df    AIC    BIC  logLik deviance   Chisq Chi Df Pr(>Chisq)
# null.model  3 1916.5 1926.1 -955.27   1910.5
# ri.model    4 1802.1 1814.8 -897.04   1794.1 116.462      1  < 2.2e-16 ***
# ric.model   6 1763.9 1783.1 -875.97   1751.9  42.139      2  7.072e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
