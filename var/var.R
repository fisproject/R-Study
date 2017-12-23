library(vars)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

data(Canada)
# Hence, prod is used as a measure of labour productivity
# e is used for employment
# U is the unemployment rate and rw assigns the real wage.

params <- VARselect(Canada, lag.max = 4)

model <- VAR(Canada, p = params$selection[1])

summary(model)
# VAR Estimation Results:
# =========================
# Endogenous variables: e, prod, rw, U
# Deterministic variables: const
# Sample size: 81
# Log Likelihood: -150.609
# Roots of the characteristic polynomial:
# 1.004 0.9283 0.9283 0.7437 0.7437 0.6043 0.6043 0.5355 0.5355 0.2258 0.2258 0.1607
# Call:
# VAR(y = timeseries, p = params$selection[1])
#
#
# Estimation results for equation e:
# ==================================
# e = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + e.l3 + prod.l3 + rw.l3 + U.l3 + const
#
#           Estimate Std. Error t value Pr(>|t|)
# e.l1       1.75274    0.15082  11.622  < 2e-16 ***
# prod.l1    0.16962    0.06228   2.723 0.008204 **
# rw.l1     -0.08260    0.05277  -1.565 0.122180
# U.l1       0.09952    0.19747   0.504 0.615915
# e.l2      -1.18385    0.23517  -5.034 3.75e-06 ***
# prod.l2   -0.10574    0.09425  -1.122 0.265858
# rw.l2     -0.02439    0.06957  -0.351 0.727032
# U.l2      -0.05077    0.24534  -0.207 0.836667
# e.l3       0.58725    0.16431   3.574 0.000652 ***
# prod.l3    0.01054    0.06384   0.165 0.869371
# rw.l3      0.03824    0.05365   0.713 0.478450
# U.l3       0.34139    0.20530   1.663 0.100938
# const   -150.68737   61.00889  -2.470 0.016029 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#
# Residual standard error: 0.3399 on 68 degrees of freedom
# Multiple R-Squared: 0.9988,	Adjusted R-squared: 0.9985
# F-statistic:  4554 on 12 and 68 DF,  p-value: < 2.2e-16
#
#
# Estimation results for equation prod:
# =====================================
# prod = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + e.l3 + prod.l3 + rw.l3 + U.l3 + const
#
#           Estimate Std. Error t value Pr(>|t|)
# e.l1      -0.14880    0.28913  -0.515   0.6085
# prod.l1    1.14799    0.11940   9.615 2.65e-14 ***
# rw.l1      0.02359    0.10117   0.233   0.8163
# U.l1      -0.65814    0.37857  -1.739   0.0866 .
# e.l2      -0.18165    0.45083  -0.403   0.6883
# prod.l2   -0.19627    0.18069  -1.086   0.2812
# rw.l2     -0.20337    0.13337  -1.525   0.1319
# U.l2       0.82237    0.47034   1.748   0.0849 .
# e.l3       0.57495    0.31499   1.825   0.0723 .
# prod.l3    0.04415    0.12239   0.361   0.7194
# rw.l3      0.09337    0.10285   0.908   0.3672
# U.l3       0.40078    0.39357   1.018   0.3121
# const   -195.86985  116.95813  -1.675   0.0986 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#
# Residual standard error: 0.6515 on 68 degrees of freedom
# Multiple R-Squared:  0.98,	Adjusted R-squared: 0.9765
# F-statistic: 277.5 on 12 and 68 DF,  p-value: < 2.2e-16
#
#
# Estimation results for equation rw:
# ===================================
# rw = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + e.l3 + prod.l3 + rw.l3 + U.l3 + const
#
#           Estimate Std. Error t value Pr(>|t|)
# e.l1    -4.716e-01  3.373e-01  -1.398    0.167
# prod.l1 -6.500e-02  1.393e-01  -0.467    0.642
# rw.l1    9.091e-01  1.180e-01   7.702 7.63e-11 ***
# U.l1    -7.941e-04  4.417e-01  -0.002    0.999
# e.l2     6.667e-01  5.260e-01   1.268    0.209
# prod.l2 -2.164e-01  2.108e-01  -1.027    0.308
# rw.l2   -1.457e-01  1.556e-01  -0.936    0.353
# U.l2    -3.014e-01  5.487e-01  -0.549    0.585
# e.l3    -1.289e-01  3.675e-01  -0.351    0.727
# prod.l3  2.140e-01  1.428e-01   1.498    0.139
# rw.l3    1.902e-01  1.200e-01   1.585    0.118
# U.l3     1.506e-01  4.592e-01   0.328    0.744
# const   -1.167e+01  1.365e+02  -0.086    0.932
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#
# Residual standard error: 0.7601 on 68 degrees of freedom
# Multiple R-Squared: 0.9989,	Adjusted R-squared: 0.9987
# F-statistic:  5239 on 12 and 68 DF,  p-value: < 2.2e-16
#
#
# Estimation results for equation U:
# ==================================
# U = e.l1 + prod.l1 + rw.l1 + U.l1 + e.l2 + prod.l2 + rw.l2 + U.l2 + e.l3 + prod.l3 + rw.l3 + U.l3 + const
#
#          Estimate Std. Error t value Pr(>|t|)
# e.l1     -0.61773    0.12508  -4.939 5.39e-06 ***
# prod.l1  -0.09778    0.05165  -1.893 0.062614 .
# rw.l1     0.01455    0.04377   0.332 0.740601
# U.l1      0.65976    0.16378   4.028 0.000144 ***
# e.l2      0.51811    0.19504   2.656 0.009830 **
# prod.l2   0.08799    0.07817   1.126 0.264279
# rw.l2     0.06993    0.05770   1.212 0.229700
# U.l2     -0.08099    0.20348  -0.398 0.691865
# e.l3     -0.03006    0.13627  -0.221 0.826069
# prod.l3  -0.01092    0.05295  -0.206 0.837180
# rw.l3    -0.03909    0.04450  -0.879 0.382733
# U.l3      0.06684    0.17027   0.393 0.695858
# const   114.36732   50.59802   2.260 0.027008 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#
# Residual standard error: 0.2819 on 68 degrees of freedom
# Multiple R-Squared: 0.9736,	Adjusted R-squared: 0.969
# F-statistic: 209.2 on 12 and 68 DF,  p-value: < 2.2e-16
#
#
#
# Covariance matrix of residuals:
#             e     prod       rw        U
# e     0.11550 -0.03161 -0.03681 -0.07034
# prod -0.03161  0.42449  0.05589  0.01494
# rw   -0.03681  0.05589  0.57780  0.03660
# U    -0.07034  0.01494  0.03660  0.07945
#
# Correlation matrix of residuals:
#            e     prod      rw        U
# e     1.0000 -0.14276 -0.1425 -0.73426
# prod -0.1428  1.00000  0.1129  0.08136
# rw   -0.1425  0.11286  1.0000  0.17084
# U    -0.7343  0.08136  0.1708  1.00000

plot(model)

pred <- predict(model, n.ahead = 20, ci = 0.95)

plot(pred)
