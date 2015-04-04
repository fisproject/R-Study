require(kernlab)

model <- ksvm(
           Species ~ ., data = iris,  # irisデータのSpeciesを他の特徴量でモデル化
           type = "C-bsvc",           # 分類方法を指定します
           kernel = "rbfdot",         # 利用するカーネル関数を指定します
           kpar = list(sigma = 0.2),  # カーネル関数のパラメーターを指定
           C = 5,                     # Cパラメーター
           prob.model = TRUE          # 予測時に予測確率を出力するためにTRUEにします
         )

unknown <- data.frame(
             Sepal.Length = c(5.1, 5.2, 6.7, 4.5, 7.0),
             Sepal.Width = c(3.5, 3.3, 4.1, 3.1, 5.0),
             Petal.Length = c(1.1, 1.5, 2.1, 6.7, 4.1),
             Petal.Width = c(0.2, 0.1, 0.3, 1.2, 1.4)
           )


acc <- 1 - cross(model)

print(acc)

# 予測結果（decision value）
result <- predict(model, unknown, type = "decision")

# 予測結果（probabilities）
predict(model, unknown, type = "probabilities")
