# install.package("randomForest")
library(randomForest)
library(rpart) 

# random sampling
n <- nrow(iris)
s <- sample(n,n*0.5)
iris.train <- iris[s,]
iris.test <- iris[-s,]

# random forest
forest <- randomForest(Species~., data=iris.train, ntree=500)
pred.forest <- predict(forest, newdata=iris.test, type="class")
table(pred.forest, iris.test[,5])

# decision tree
tree <- rpart(Species~.,data=iris.train)
pred.rpart <- predict(tree, iris.test, type="class")
table(pred.rpart, iris.test[,5])

# importance
getTree(forest, 1, labelVar=TRUE)
varImpPlot(forest)

# plot
plot(forest)