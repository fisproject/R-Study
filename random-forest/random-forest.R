library(randomForest)
library(RColorBrewer)

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
# pred.model   setosa versicolor virginica
#   setosa         30          0         0
#   versicolor      0         16         2
#   virginica       0          1        26

getTree(model, 1, labelVar=TRUE)

importance(model)
# MeanDecreaseGini
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

# Grid Search
tuneRF(iris.train[,-5], iris.train[,5], doBest=T)
# mtry = 2  OOB error = 6.67%
# Searching left ...
# mtry = 1 	OOB error = 6.67%
# 0 0.05
# Searching right ...
# mtry = 4 	OOB error = 5.33%
# 0.2 0.05
#
# Call:
#  randomForest(x = x, y = y, mtry = res[which.min(res[, 2]), 1])
#                Type of random forest: classification
#                      Number of trees: 500
# No. of variables tried at each split: 4
#
#         OOB estimate of  error rate: 6.67%
# Confusion matrix:
#            setosa versicolor virginica class.error
# setosa         20          0         0  0.00000000
# versicolor      0         31         2  0.06060606
# virginica       0          3        19  0.13636364

model.tune <- randomForest(
  Species ~ .,
  data=iris.train,
  mtry=4 # Number of variables randomly sampled as candidates at each split
)

pred.model.tune <- predict(
  model.tune,
  newdata=iris.test
)

table(pred.model.tune, iris.test[,5])
# pred.model.num setosa versicolor virginica
#     setosa         30          0         0
#     versicolor      0         16         2
#     virginica       0          1        26


importance(model.tune)
# MeanDecreaseGini
# Sepal.Length        0.7495972
# Sepal.Width         0.7205969
# Petal.Length       23.5112116
# Petal.Width        23.0789143

split.screen(c(2,1))
split.screen(c(1,3), screen = 2)
screen(3); partialPlot(model.tune, iris.train, x.var=Petal.Length)
screen(4); partialPlot(model.tune, iris.train, x.var=Petal.Width)
screen(5); partialPlot(model.tune, iris.train, x.var=Sepal.Length)
split.screen(c(2,1), screen = 1)
screen(1); plot(model.tune)
close.screen(all=T)
