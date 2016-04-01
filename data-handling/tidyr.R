require(dplyr)
require(tidyr)

data("gapminder", package = "gapminder")

# total 142 countries
gapminder %>% glimpse()
# Observations: 1,704
# Variables: 6
# $ country   (fctr) Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanis...
# $ continent (fctr) Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Europe, Europe...
# $ year      (int) 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007, 1952, 1957, 196...
# $ lifeExp   (dbl) 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.854, 40.822, 41.674, 41.763, 42.129,...
# $ pop       (int) 8425333, 9240934, 10267083, 11537966, 13079460, 14880372, 12881816, 13867957, 16317921,...
# $ gdpPercap (dbl) 779.4453, 820.8530, 853.1007, 836.1971, 739.9811, 786.1134, 978.0114, 852.3959, 649.341...

nest_by_country <- gapminder %>% group_by(continent, country) %>% nest()

position <- gapminder$country %>% factor() %>% levels() %>% grep("Japan", .)

japan <- nest_by_country$data[[position]]

# linear model each country
(do_by_country <- gapminder %>%
    group_by(country) %>%
    do(data = lm(lifeExp ~ year + gdpPercap + pop , data = .) %>% broom::tidy()))

lm.japan <- do_by_country$data[[position]]
summary(lm.japan)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept) -4.133e+02  1.831e+02  -2.257 0.053956 .
# year         2.276e-01  9.366e-02   2.430 0.041228 *
# gdpPercap   -3.379e-04  2.046e-04  -1.651 0.137279
# pop          3.908e-07  7.328e-08   5.332 0.000701 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.6873 on 8 degrees of freedom
# Multiple R-squared:  0.9919,	Adjusted R-squared:  0.9888
# F-statistic: 324.7 on 3 and 8 DF,  p-value: 1.08e-08

lm.all <- do_by_country %>% unnest()
