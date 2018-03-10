library(tibble)
library(dplyr)

sessionInfo()
# R version 3.3.1 (2016-06-21)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: OS X 10.13.1 (unknown)
#
# locale:
# [1] ja_JP.UTF-8/ja_JP.UTF-8/ja_JP.UTF-8/C/ja_JP.UTF-8/ja_JP.UTF-8
#
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
# [1] bindrcpp_0.2 dplyr_0.7.4  tibble_1.4.2
#
# loaded via a namespace (and not attached):
#  [1] magrittr_1.5    assertthat_0.1  R6_2.2.0        cli_1.0.0
#  [5] tools_3.3.1     pillar_1.2.1    glue_1.2.0      rstudioapi_0.7
#  [9] crayon_1.3.4    Rcpp_0.12.7     utf8_1.1.3      pkgconfig_2.0.1
# [13] rlang_0.2.0     bindr_0.1

x <- c(NA, NaN, Inf, -Inf)
is.na(x)
# [1]  TRUE  TRUE FALSE FALSE
is.nan(x)
# [1] FALSE  TRUE FALSE FALSE
is.infinite(x)
# [1] FALSE FALSE  TRUE  TRUE
is.finite(x)
# [1] FALSE FALSE FALSE FALSE

df <- tibble(x = c(1, 2, 3), y = c(1, NA, 3))
glimpse(df)
# Observations: 3
# Variables: 2
# $ x <dbl> 1, 2, 3
# $ y <dbl> 1, NA, 3

df_summary <- df %>%
  group_by(x) %>%
  summarise_all(funs(min = min, max = max, med = median, mn = mean, sd = sd),
                na.rm = TRUE)

df_summary
# A tibble: 3 x 6
#       x   min   max   med    mn    sd
#   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1    1.    1.    1.    1.    1.    NA
# 2    2.  Inf  -Inf    NA   NaN     NA
# 3    3.    3.    3.    3.    3.    NA

# Replace Inf/-Inf/NaN with NA
df_summary %>%
  mutate_all(funs(ifelse(is.nan(.), NA, .))) %>%
  mutate_all(funs(ifelse(is.infinite(.), NA, .)))
# A tibble: 3 x 6
# x   min   max   med    mn    sd
# <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1    1.    1.    1.    1.    1.    NA
# 2    2.   NA    NA    NA    NA     NA
# 3    3.    3.    3.    3.    3.    NA

# or below
df_summary %>%
  mutate_all(funs(ifelse(is.finite(.), ., NA)))
# # A tibble: 3 x 6
#       x   min   max   med    mn sd
#   <dbl> <dbl> <dbl> <dbl> <dbl> <lgl>
# 1    1.    1.    1.    1.    1. NA
# 2    2.   NA    NA    NA    NA  NA
# 3    3.    3.    3.    3.    3. NA
