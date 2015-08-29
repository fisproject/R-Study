require(Matching)

data(lalonde)
head(lalonde)
# age educ black hisp married nodegr re74 re75     re78 u74 u75 treat
# 1  37   11     1    0       1      1    0    0  9930.05   1   1     1
# 2  22    9     0    1       0      1    0    0  3595.89   1   1     1
# 3  30   12     1    0       0      0    0    0 24909.50   1   1     1
# 4  27   11     1    0       0      1    0    0  7506.15   1   1     1
# 5  33    8     1    0       0      1    0    0   289.79   1   1     1
# 6  22    9     1    0       0      1    0    0  4056.49   1   1     1

model.glm <- glm(treat ~., data=lalonde[,-9], family=binomial)
summary(model.glm)
# Deviance Residuals:
#     Min       1Q   Median       3Q      Max
# -1.4884  -0.9934  -0.8708   1.2242   1.7403
#
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)
# (Intercept)  1.622e+00  1.102e+00   1.472   0.1410
# age          8.276e-03  1.452e-02   0.570   0.5687
# educ        -8.282e-02  7.230e-02  -1.145   0.2520
# black       -2.216e-01  3.684e-01  -0.601   0.5476
# hisp        -8.557e-01  5.128e-01  -1.669   0.0952 .
# married      1.960e-01  2.784e-01   0.704   0.4813
# nodegr      -8.981e-01  3.146e-01  -2.855   0.0043 **
# re74        -4.466e-05  3.010e-05  -1.483   0.1380
# re75         2.924e-05  4.788e-05   0.611   0.5414
# u74         -1.927e-01  3.765e-01  -0.512   0.6088
# u75         -3.369e-01  3.213e-01  -1.048   0.2945
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 604.20  on 444  degrees of freedom
# Residual deviance: 584.26  on 434  degrees of freedom
# AIC: 606.26
#
# Number of Fisher Scoring iterations: 4

model.lm <- lm(lalonde$re78 ~ lalonde$treat + model.glm$fitted)
summary(model.lm)
# Call:
# lm(formula = lalonde$re78 ~ lalonde$treat + model.glm$fitted)
#
# Residuals:
#    Min     1Q Median     3Q    Max
#  -6948  -4532  -1798   2813  54023
#
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)        3475.3     1289.5   2.695   0.0073 **
# lalonde$treat      1674.5      647.4   2.587   0.0100 *
# model.glm$fitted   2716.5     3078.0   0.883   0.3779
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 6581 on 442 degrees of freedom
# Multiple R-squared:  0.01955,	Adjusted R-squared:  0.01511
# F-statistic: 4.407 on 2 and 442 DF,  p-value: 0.01273

mout <- Match(
    Y=lalonde$re78,
    Tr=lalonde$treat,
    X=model.glm$fitted,
  )

summary(mout)
# Estimate...  2138.6
# AI SE......  797.76
# T-stat.....  2.6807
# p.val......  0.0073468
#
# Original number of observations..............  445
# Original number of treated obs...............  185
# Matched number of observations...............  185
# Matched number of observations  (unweighted).  322

mout <- Match(
    Y=lalonde$re78,
    Tr=lalonde$treat,
    X=model.glm$fitted,
    estimand="ATT",
    caliper=T
  )

summary(mout)
# Estimate...  2138.6
# AI SE......  797.76
# T-stat.....  2.6807
# p.val......  0.0073468
#
# Original number of observations..............  445
# Original number of treated obs...............  185
# Matched number of observations...............  185
# Matched number of observations  (unweighted).  322
#
# Caliper (SDs)........................................   TRUE
# Number of obs dropped by 'exact' or 'caliper'  0


MatchBalance(
    treat ~ .,
    match.out=mout,
    nboots=1000,
    data=lalonde[,-9]
  )
# Before Matching Minimum p.value: 0.0020368
# Variable Name(s): nodegr  Number(s): 6
#
# After Matching Minimum p.value: 0.061
# Variable Name(s): re75  Number(s): 8
