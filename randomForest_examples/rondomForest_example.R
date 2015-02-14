# install.packages("randomForest")
library(randomForest)
library(rpart) 
library(RColorBrewer)

# random sampling
n <- nrow(iris)
s <- sample(n, n*0.5)
iris.train <- iris[s,]
iris.test <- iris[-s,]

# random forest
forest <- randomForest(Species~., data=iris.train, ntree=500, proximity=TRUE)
pred.forest <- predict(forest, newdata=iris.test, type="class")
table(pred.forest, iris.test[,5])

# decision tree
tree <- rpart(Species~., data=iris.train)
pred.rpart <- predict(tree, iris.test, type="class")
table(pred.rpart, iris.test[,5])

# importance
getTree(forest, 1, labelVar=TRUE)
varImpPlot(forest)
MDSplot(forest, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))

# report
split.screen(c(2,1))
split.screen(c(1,3), screen = 2)
screen(3); partialPlot(forest, iris, Petal.Length, "setosa")
screen(4); partialPlot(forest, iris, Petal.Length, "versicolor")
screen(5); partialPlot(forest, iris, Petal.Length, "virginica")
split.screen(c(2,1), screen = 1)
screen(1); plot(forest) 
close.screen(all=T)