require(randomForest)
require(RColorBrewer)

set.seed(1500)

# random sampling
n <- nrow(iris)
s <- sample(n, n * 0.5)
iris.train <- iris[s,]
iris.test <- iris[-s,]

model <- randomForest(
  Species ~ .,
  data=iris.train,
  ntree=500,
  proximity=TRUE
)

pred.model <- predict(
  model,
  newdata=iris.test,
  type='class'
)

table(pred.model, iris.test[,5])

getTree(model, 1, labelVar=TRUE)
# pred.model   setosa versicolor virginica
#   setosa         30          0         0
#   versicolor      0         16         2
#   virginica       0          1        26

importance(model)
#                 MeanDecreaseGini
# Sepal.Length         3.590923
# Sepal.Width          2.366265
# Petal.Length        21.485794
# Petal.Width         20.524631

# MDG
varImpPlot(model)

MDSplot(model, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))

split.screen(c(2,1))
split.screen(c(1,3), screen = 2)
screen(3); partialPlot(model, iris, Petal.Length, 'setosa')
screen(4); partialPlot(model, iris, Petal.Length, 'versicolor')
screen(5); partialPlot(model, iris, Petal.Length, 'virginica')
split.screen(c(2,1), screen = 1)
screen(1); plot(model)
close.screen(all=T)
