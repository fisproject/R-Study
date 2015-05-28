require(xgboost)

# k-fold
data(iris)
num <- sample(nrow(iris), 150)
k <- 2
data <- split(iris[num,], 1:k)

model <- xgboost(
    params=list(objective="multi:softmax"),
    eval_metric="mlogloss",
    num_class=3,
    data=as.matrix(data$"1"[,1:4]),
    label=as.integer(data$"1"[,5])-1,
    nrounds=500
  )

pred <- predict(model, as.matrix(data$"2"[,1:4]))
table(as.integer(data$"2"[,5])-1, pred)
# pred
#   0  1  2
# 0 28  0  0
# 1  0 24  3
# 2  0  2 18

sum(ifelse(as.integer(data$"2"[,5])-1 == pred, 0, 1)) / (nrow(iris)/k)
# [1] 0.06666667

imp <- xgb.importance(names(iris), model=model)
print(imp)
# Feature        Gain      Cover  Frequence
# 1: Petal.Length 0.882104244 0.52692315 0.44000000
# 2:  Petal.Width 0.103582149 0.25266661 0.24153846
# 3:  Sepal.Width 0.008033523 0.06575587 0.09384615
# 4: Sepal.Length 0.006280085 0.15465438 0.22461538
