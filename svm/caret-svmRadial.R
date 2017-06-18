require(caret)
require(kernlab)
require(ggplot2)
require(e1071)

set.seed(1500)

grid <- expand.grid(
  C = ((2:10)*0.5),
  sigma = ((1:10)*0.01)
)

ctrl <- trainControl(
  method = "cv",
  savePred = T,
  classProb = T,
  number = 3
)

svmFit <- train(
  Species ~ .,
  data = iris,
  method = "svmRadial", # gaussian kernel
  trace = T,
  trControl = ctrl,
  tuneGrid = grid
)

# eval
print(svmFit)
# Support Vector Machines with Radial Basis Function Kernel
#
# 150 samples
#   4 predictor
#   3 classes: 'setosa', 'versicolor', 'virginica'
#
# No pre-processing
# Resampling: Cross-Validated (3 fold)
# Summary of sample sizes: 102, 99, 99
# Resampling results across tuning parameters:
#
#   C    sigma  Accuracy   Kappa      Accuracy SD   Kappa SD
#   1.0  0.01   0.8864379  0.8296569  0.0139368645  0.020905297
#   1.0  0.02   0.9195261  0.8792892  0.0227404668  0.034110700
#   1.0  0.03   0.9530229  0.9295343  0.0134432048  0.020164807
#   1.0  0.04   0.9665033  0.9497549  0.0120903992  0.018135599
#   1.0  0.05   0.9665033  0.9497549  0.0120903992  0.018135599
#   1.0  0.06   0.9599673  0.9399510  0.0014150742  0.002122611
#   ...

tune <- svmFit$bestTune
svmFit$finalModel

head(svmFit$pred)

post <- postResample(svmFit$pred$pred, svmFit$pred$obs)
R2(svmFit$pred$pred, svmFit$pred$obs, formula = "traditional", na.rm = FALSE)

# results
res.C <- svmFit$results$C
res.sigma <- svmFit$results$sigma
res.acc <- svmFit$results$Accuracy

res <- data.frame(svmFit$pred)

p <- ggplot(svmFit$results, aes(x = C, y = sigma))
p + geom_point(aes(colour = Accuracy), size = 10) + scale_colour_gradient(low = "#0068b7", high = "#f39800")

model <- ksvm(
  Species ~ .,
  data = iris,
  type = "C-svc",
  kernel = "rbfdot",
  C = tune$C,
  kpar=list(sigma = res.sigma)
)

iris.res <- predict(model, iris)
table(iris.res, iris$Species)
# iris.res     setosa versicolor virginica
#   setosa         50          0         0
#   versicolor      0         49         5
#   virginica       0          1        45
