require(dplyr)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- iris

# SQL : where
filtered <- d %>% filter(Sepal.Length > 6.5)

# SQL : order-by
arranged <- d %>% arrange(desc(Sepal.Length))

# SQL : select
selected <- d %>% select(Sepal.Length) %>% arrange(desc(Sepal.Length)) %>% head(10)
# Sepal.Length
# 1           7.9
# 2           7.7
# 3           7.7
# 4           7.7
# 5           7.7
# 6           7.6
# 7           7.4
# 8           7.3
# 9           7.2
# 10          7.2

# SQL : count ,max , min
summarised <- d %>% summarise(
  max=max(Sepal.Length),
  min=min(Sepal.Length),
  mean=mean(Sepal.Length)
)
#   max min  mean
# 1 7.9 4.3 5.843333

# Add column
d.new <- d %>% mutate(
  is_virginica=ifelse(Species == "virginica", 1, 0)
)

# SQL : group-by
group <- d.new %>% group_by(Species) %>% summarise_each(funs(mean))
# Source: local data frame [3 x 6]
#
#      Species Sepal.Length Sepal.Width Petal.Length Petal.Width
#       (fctr)        (dbl)       (dbl)        (dbl)       (dbl)
# 1     setosa        5.006       3.428        1.462       0.246
# 2 versicolor        5.936       2.770        4.260       1.326
# 3  virginica        6.588       2.974        5.552       2.026
# Variables not shown: is_virginica (dbl)
