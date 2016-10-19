require(dplyr)

# Sort
sorted <- iris %>% arrange(Sepal.Length)

# Lag
sorted.lag <- sorted %>% mutate(Sepal.Length.Diff = Sepal.Length - lag(Sepal.Length, n = 1))

# Summarise a data frame
diff <- iris %>% summarise(diff = max(Sepal.Length) - min(Sepal.Length))

# Rename columns
iris.renamed <- iris %>% rename(type = Species)

# Add new column
iris.new <- iris %>% mutate(
  is_setosa = ifelse(Species == "setosa", 1, 0),
  is_versicolor = ifelse(Species == "versicolor", 1, 0),
  is_virginica = ifelse(Species == "virginica", 1, 0)
)

# Rank
ranking <- iris %>% mutate(
  percent_rank = percent_rank(Sepal.Length),
  row_number = row_number(Sepal.Length),
  min_rank = min_rank(desc(Sepal.Length)),
  cume_dist = cume_dist(Sepal.Length),
  dense_rank = dense_rank(Sepal.Length)
)


# SQL: WHERE
filtered <- iris %>% filter(Sepal.Length > 6.5)

# SQL: SELECT
selected <- iris %>% select(Sepal.Length) %>%
  arrange(desc(Sepal.Length)) %>%
  head(10)

# SQL: ORDER BY
arranged <- iris %>% arrange(desc(Sepal.Length))

# SQL: COUNT, MAX, MIN
summarised <- iris %>% summarise(
  max = max(Sepal.Length),
  min = min(Sepal.Length),
  mean = mean(Sepal.Length)
)

# SQL: GROUP BY
group <- iris.new %>%
  group_by(Species) %>%
  summarise_each(funs(mean))
# Source: local data frame [3 x 6]
#
#      Species Sepal.Length Sepal.Width Petal.Length Petal.Width
#       (fctr)        (dbl)       (dbl)        (dbl)       (dbl)
# 1     setosa        5.006       3.428        1.462       0.246
# 2 versicolor        5.936       2.770        4.260       1.326
# 3  virginica        6.588       2.974        5.552       2.026
# Variables not shown: is_virginica (dbl)
