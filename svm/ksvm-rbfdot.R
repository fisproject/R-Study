require(kernlab)

set.seed(1500)

model <- ksvm(
  Species ~ .,
  data = iris,
  type = "C-bsvc", # method
  kernel = "rbfdot",
  kpar = list(sigma=0.2), # kernel param gamma
  C = 5, # margin param C
  prob.model=TRUE # probabilities
)

unknown.data <- data.frame(
  Sepal.Length = c(5.1, 5.2, 6.7, 4.5, 7.0),
  Sepal.Width = c(3.5, 3.3, 4.1, 3.1, 5.0),
  Petal.Length = c(1.1, 1.5, 2.1, 6.7, 4.1),
  Petal.Width = c(0.2, 0.1, 0.3, 1.2, 1.4)
)

acc <- 1 - cross(model)

# decision value
result <- predict(model, unknown.data, type = "decision")

# probabilities
predict(model, unknown.data, type = "probabilities")
#         setosa  versicolor   virginica
# [1,] 0.98261264 0.009862578 0.007524781
# [2,] 0.97803365 0.013482015 0.008484336
# [3,] 0.86325818 0.093594021 0.043147802
# [4,] 0.08111659 0.348235709 0.570647698
# [5,] 0.44967841 0.244543068 0.305778522
