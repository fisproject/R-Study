library(dplyr)
library(tibble)

# Retain only unique/distinct rows
species_uniq <- iris %>%
  distinct(Species, .keep_all = TRUE)

# Lag and Diff
sorted_lag <- iris %>%
  mutate(Sepal.Length.lag = lag(Sepal.Length, n = 1),
         Sepal.Length.diff = Sepal.Length - lag(Sepal.Length, n = 1))

# Summarise a data frame
diff_length <- iris %>%
  summarise(diff = max(Sepal.Length) - min(Sepal.Length))

# Rename columns
renamed <- iris %>%
  rename(sepal_Length = Sepal.Length,
         sepal_width = Sepal.Width,
         type = Species)

# Add new column
iris_bool <- iris %>%
  mutate(is_setosa = if_else(Species == "setosa", T, F),
         is_versicolor = if_else(Species == "versicolor", T, F),
         is_virginica = if_else(Species == "virginica", T, F))

# Rank
ranking <- iris %>%
  mutate(percent_rank = percent_rank(Sepal.Length),
         row_number = row_number(Sepal.Length),
         min_rank = min_rank(desc(Sepal.Length)),
         cume_dist = cume_dist(Sepal.Length),
         dense_rank = dense_rank(Sepal.Length))

# SQL: WHERE
filtered_length <- iris %>%
  filter(Sepal.Length > 6.5)

iris %>%
  filter(Sepal.Length == 6.2 & Sepal.Width == 2.2) %>%
  count()
# # A tibble: 1 × 1
#       n
#   <int>
# 1     1

iris %>%
  filter(Sepal.Length == 6.2 | Sepal.Width == 2.2) %>%
  count()
# # A tibble: 1 × 1
#       n
#   <int>
# 1     6

iris %>%
  filter(is.na(Species)) %>%
  count()
# # A tibble: 1 × 1
#       n
#   <int>
# 1     0

filtered_species <- iris %>%
  filter(Species %in% c("setosa", "versicolor"))

# SQL: SELECT
iris %>%
  select(Sepal.Length) %>%
  arrange(desc(Sepal.Length)) %>%
  head(3)

iris %>%
  select(-Sepal.Length) %>%
  arrange(desc(Sepal.Width)) %>%
  head(3)

iris %>%
  select(starts_with("Petal")) %>%
  head(3)
# Petal.Length Petal.Width
# 1          1.4         0.2
# 2          1.4         0.2
# 3          1.3         0.2

iris %>%
  select(ends_with(".Length")) %>%
  head(3)
# Sepal.Length Petal.Length
# 1          5.1          1.4
# 2          4.9          1.4
# 3          4.7          1.3

# SQL: ORDER BY
sorted <- iris %>%
  arrange(Sepal.Length)

sorted_desc <- iris %>%
  arrange(desc(Sepal.Length))

# SQL: COUNT, MAX, MIN
iris %>%
  summarise(n = n(),
            max = max(Sepal.Length, na.rm = TRUE),
            min = min(Sepal.Length, na.rm = TRUE),
            mean = mean(Sepal.Length, na.rm = TRUE),
            sd = sd(Sepal.Length, na.rm = TRUE))
# n max min     mean        sd
# 1 150 7.9 4.3 5.843333 0.8280661

# SQL: GROUP BY
iris %>%
  group_by(Species) %>%
  summarise_each(funs(mean))
# Source: local data frame [3 x 6]
#
#      Species Sepal.Length Sepal.Width Petal.Length Petal.Width
#       (fctr)        (dbl)       (dbl)        (dbl)       (dbl)
# 1     setosa        5.006       3.428        1.462       0.246
# 2 versicolor        5.936       2.770        4.260       1.326
# 3  virginica        6.588       2.974        5.552       2.026

# SQL: JOIN
x <- tibble(key = c(1, 2, 3), x = c("x1", "x2", "x3"))
y <- tibble(key = c(1, 2, 4), y = c("y1", "y2", "y3"))

x %>%
  inner_join(y, by = "key")
# # A tibble: 2 × 3
#     key     x     y
#   <dbl> <chr> <chr>
# 1     1    x1    y1
# 2     2    x2    y2

x %>%
  left_join(y, by = "key")
# # A tibble: 3 × 3
#     key     x     y
#   <dbl> <chr> <chr>
# 1     1    x1    y1
# 2     2    x2    y2
# 3     3    x3  <NA>

x %>%
  right_join(y, by = "key")
# # A tibble: 3 × 3
#     key     x     y
#   <dbl> <chr> <chr>
# 1     1    x1    y1
# 2     2    x2    y2

x %>%
  full_join(y, by = "key")
# # A tibble: 4 × 3
#     key     x     y
#   <dbl> <chr> <chr>
# 1     1    x1    y1
# 2     2    x2    y2
# 3     3    x3  <NA>
# 4     4  <NA>    y3

x %>%
  semi_join(y, by = "key")
# # A tibble: 2 × 2
#     key     x
#   <dbl> <chr>
# 1     1    x1
# 2     2    x2

x %>%
  anti_join(y, by = "key")
# # A tibble: 1 × 2
#     key     x
#   <dbl> <chr>
# 1     3    x3

y %>%
  anti_join(x, by = "key")
# # A tibble: 1 × 2
#     key     y
#   <dbl> <chr>
# 1     4    y3

# Set operations
df1 <- tibble(x = c(1, 2, 3), y = c(1, 2, 3))
df2 <- tibble(x = c(1, 2, 4), y = c(1, 2, 4))

df1 %>%
  intersect(df2)
# # A tibble: 2 × 2
#       x     y
#   <dbl> <dbl>
# 1     1     1
# 2     2     2

df1 %>%
  union(df2)
# # A tibble: 4 × 2
#       x     y
#   <dbl> <dbl>
# 1     4     4
# 2     3     3
# 3     2     2
# 4     1     1

df1 %>%
  setdiff(df2)
# # A tibble: 1 × 2
#       x     y
#   <dbl> <dbl>
# 1     3     3

# Transpose matrix
t(df1) %>%
  as_tibble()
# # A tibble: 2 x 3
# V1    V2    V3
# <dbl> <dbl> <dbl>
#   1     1     2     3
#   2     1     2     3

# Transpose with column name
cbind(name = names(df1), t(df1)) %>%
  as_tibble()
# # A tibble: 2 x 4
#   name  V2    V3    V4   
#   <chr> <chr> <chr> <chr>
# 1 x     1     2     3    
# 2 y     1     2     3    
