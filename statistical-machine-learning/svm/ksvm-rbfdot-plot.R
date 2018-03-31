library(kernlab)

set.seed(1500)

d <- data.frame(iris[51:150,3:4], y = as.character.factor(iris[51:150,5])) # versicolor and virginica
d$y <- setattr(d$y,"levels", c("versicolor", "virginica"))

iris.ksvm <- ksvm(
  y ~ ., # y is Species
  data = d,
  kernel = "rbfdot",
  kpar = list(sigma=0.2), # kernel param sigma
  C = 5, # margin param C
  cross = 3
)

print(iris.ksvm)

plot(iris.ksvm, data = d[,1:2])

table(d$y, predict(iris.ksvm, d[,1:2]))
#             versicolor virginica
# versicolor         47         3
# virginica           3        47
