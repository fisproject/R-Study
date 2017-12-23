library(MASS)

sep <- 2 * (1:75) - 1
iris.train <- iris[sep,]
iris.test.x <- iris[-sep, -5]
iris.test.y <- iris[-sep, 5]

# linear discriminant
model <- lda(Species ~ ., data = iris.train)

model$means %*% model$scaling

pred.train <- predict(model, iris.test.x)

table(iris.train$Species, pred.train$class)
#               setosa versicolor virginica
# setosa         25          0         0
# versicolor      0         24         1
# virginica       0          1        24


pred.test <- predict(model, iris.test.x)

table(iris.test.y, pred.test$class)
# iris.test.y  setosa versicolor virginica
# setosa         25          0         0
# versicolor      0         24         1
# virginica       0          2        23
