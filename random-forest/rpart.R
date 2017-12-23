library(rpart)
library(RColorBrewer)

set.seed(1500)

# random sampling
n <- nrow(iris)
s <- sample(n, n * 0.5)
iris.train <- iris[s,]
iris.test <- iris[-s,]

# decision tree
tree <- rpart(
  Species ~ .,
  data=iris.train
)

pred.tree <- predict(
  tree,
  iris.test,
  type='class'
)

table(pred.tree, iris.test[,5])
#   pred.tree   setosa versicolor virginica
#   setosa         30          0         0
#   versicolor      0         14         1
#   virginica       0          3        27
