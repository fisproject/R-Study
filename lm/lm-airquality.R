require(car)
require(ppcor)

# airquality : Daily air quality measurements in New York. May to September 1973.
air <- na.omit(airquality)
head(air)
# Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
# 3    12     149 12.6   74     5   3
# 4    18     313 11.5   62     5   4
# 7    23     299  8.6   65     5   7
# 8    19      99 13.8   59     5   8

pcor(air)
# $estimate
#              Ozone     Solar.R        Wind        Temp       Month         Day
# Ozone    1.0000000  0.20505146 -0.44897864  0.55975584 -0.19236023  0.11559389
# Solar.R  0.2050515  1.00000000  0.11427512  0.14942920 -0.17190775 -0.04728812
# Wind    -0.4489786  0.11427512  1.00000000 -0.07988093 -0.07467615  0.05413314
# Temp     0.5597558  0.14942920 -0.07988093  1.00000000  0.43771581 -0.12748877
# Month   -0.1923602 -0.17190775 -0.07467615  0.43771581  1.00000000  0.04833785
# Day      0.1155939 -0.04728812  0.05413314 -0.12748877  0.04833785  1.00000000

air.lm <- lm(Ozone ~ Solar.R + Wind + Temp, data = air)
summary(air.lm)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) -64.34208   23.05472  -2.791  0.00623 **
# Solar.R       0.05982    0.02319   2.580  0.01124 *
# Wind         -3.33359    0.65441  -5.094 1.52e-06 ***
# Temp          1.65209    0.25353   6.516 2.42e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 21.18 on 107 degrees of freedom
# Multiple R-squared:  0.6059,	Adjusted R-squared:  0.5948
# F-statistic: 54.83 on 3 and 107 DF,  p-value: < 2.2e-16

vif(air.lm)
# air$Solar.R    air$Wind    air$Temp
#    1.095253    1.329070    1.431367

step(air.lm)
# Start:  AIC=681.71
# Ozone ~ Solar.R + Wind + Temp
#
#           Df Sum of Sq   RSS    AIC
# <none>                 48003 681.71
# - Solar.R  1    2986.2 50989 686.41
# - Wind     1   11641.6 59644 703.82
# - Temp     1   19049.9 67053 716.81
#
# Call:
# lm(formula = Ozone ~ Solar.R + Wind + Temp, data = air)
#
# Coefficients:
# (Intercept)      Solar.R         Wind         Temp
#   -64.34208      0.05982     -3.33359      1.65209

# diagnostic plots
par(mfrow = c(2,2))
plot(air.lm)
