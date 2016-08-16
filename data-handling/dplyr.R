require(dplyr)

# SQL: where
filtered <- iris %>% filter(Sepal.Length > 6.5)

# SQL: order-by
arranged <- iris %>% arrange(desc(Sepal.Length))

# SQL: select
selected <- iris %>% select(Sepal.Length) %>% arrange(desc(Sepal.Length)) %>% head(10)

# SQL: count ,max , min
summarised <- iris %>% summarise(
  max = max(Sepal.Length),
  min = min(Sepal.Length),
  mean = mean(Sepal.Length)
)

# Sort
sorted <- iris %>% arrange(Sepal.Length)

# Lag
sorted.new <- sorted %>% mutate(Sepal.Length.Diff = Sepal.Length - lag(Sepal.Length, n = 1))

# Add column
iris.new <- iris %>% mutate(
  is_setosa = ifelse(Species == "setosa", 1, 0),
  is_versicolor = ifelse(Species == "versicolor", 1, 0),
  is_virginica = ifelse(Species == "virginica", 1, 0)
)

# SQL: group-by
group <- iris.new %>% group_by(Species) %>% summarise_each(funs(mean))
# Source: local data frame [3 x 6]
#
#      Species Sepal.Length Sepal.Width Petal.Length Petal.Width
#       (fctr)        (dbl)       (dbl)        (dbl)       (dbl)
# 1     setosa        5.006       3.428        1.462       0.246
# 2 versicolor        5.936       2.770        4.260       1.326
# 3  virginica        6.588       2.974        5.552       2.026
# Variables not shown: is_virginica (dbl)

# Rank
ranking <- iris %>% mutate(
  percent_rank = percent_rank(Sepal.Length),
  row_number = row_number(Sepal.Length),
  min_rank = min_rank(desc(Sepal.Length)),
  cume_dist = cume_dist(Sepal.Length),
  dense_rank = dense_rank(Sepal.Length)
)
