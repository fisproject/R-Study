require(foreign)
require(ggplot2)

# Panel data
panel = read.dta("http://dss.princeton.edu/training/Panel101.dta")

head(panel)
# country year           y y_bin        x1         x2          x3   opinion
# 1       A 1990  1342787840     1 0.2779036 -1.1079559  0.28255358 Str agree
# 2       A 1991 -1899660544     0 0.3206847 -0.9487200  0.49253848     Disag
# 3       A 1992   -11234363     0 0.3634657 -0.7894840  0.70252335     Disag
# 4       A 1993  2645775360     1 0.2461440 -0.8855330 -0.09439092     Disag
# 5       A 1994  3008334848     1 0.4246230 -0.7297683  0.94613063     Disag
# 6       A 1995  3229574144     1 0.4772141 -0.7232460  1.02968037 Str agree

# after 1994
panel$postperiod = ifelse(panel$year >= 1994, 1, 0)
panel$treated = ifelse(panel$country == "E" | panel$country == "F" | panel$country == "G", 1, 0)
panel$did = panel$postperiod * panel$treated

g <- ggplot(panel, aes(x = year, y = y, group = country, colour = country)) +
    geom_line() + geom_point(aes(shape = factor(treated)), size = 5) +
    geom_vline(xintercept = 1994)
plot(g)


# difference in differences
model.did = lm(y ~ treated + postperiod + did, data = panel)

# p-value 0.0882 : The effect is significant at 10% with the treatment having a negative effect.
summary(model.did)
# Call:
# lm(formula = y ~ treated + time + did, data = panel)
#
# Residuals:
#        Min         1Q     Median         3Q        Max
# -9.768e+09 -1.623e+09  1.167e+08  1.393e+09  6.807e+09
#
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept)  3.581e+08  7.382e+08   0.485   0.6292
# treated      1.776e+09  1.128e+09   1.575   0.1200
# time         2.289e+09  9.530e+08   2.402   0.0191 *
# did         -2.520e+09  1.456e+09  -1.731   0.0882 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.953e+09 on 66 degrees of freedom
# Multiple R-squared:  0.08273,	Adjusted R-squared:  0.04104
# F-statistic: 1.984 on 3 and 66 DF,  p-value: 0.1249
