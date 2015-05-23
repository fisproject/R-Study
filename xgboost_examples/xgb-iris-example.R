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
# 0 26  0  0
# 1  0 25  0
# 2  0  1 23

imp <- xgb.importance(names(iris), model=model)
print(imp)
# Feature       Gain     Cover      Frequence
# 1:  Petal.Width 0.53568637 0.2162732 0.1150855
# 2:      Species 0.41282008 0.1875520 0.1454121
# 3: Petal.Length 0.03220073 0.3496323 0.4494557
# 4:  Sepal.Width 0.01929283 0.2465425 0.2900467
