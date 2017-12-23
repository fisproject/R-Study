library(class)

iris <- data.frame(iris)

# train
train <- rbind(iris[1:25, 1:4], iris[51:75, 1:4], iris[101:125, 1:4])
train.label <- factor(c(iris[1:25, 5], iris[51:75, 5], iris[101:125, 5]))

# test
test <- rbind(iris[26:50, 1:4], iris[76:100, 1:4], iris[126:150, 1:4])
test.label <- factor(c(iris[26:50, 5], iris[76:100, 5], iris[126:150 ,5]))

# knn
res <- knn(train, test, train.label, k=3, prob=FALSE)
summary(res)

# accuracy
sum(res==test.label)/length(test.label)
