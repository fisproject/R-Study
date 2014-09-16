# install.packages("caret")
library(caret)
library(e1071)
library(kernlab)

grid <- expand.grid(.C =((2:20)*0.5),.sigma = ((1:10)*0.01))
ctrl <- trainControl(method = "cv", savePred=T, classProb=T, number=3)

# svmRadial : gaussian kernel
svmFit<-train(Species~.,data=iris,method="svmRadial",trace=T, trControl = ctrl ,tuneCount = 5)

# eval
print(svmFit)

tune <- svmFit$bestTune
svmFit$finalModel

head(svmFit$pred)
print(svmFit$pred)

post <- postResample(svmFit$pred$pred, svmFit$pred$obs)
R2(svmFit$pred$pred, svmFit$pred$obs, formula = "traditional", na.rm = FALSE)

# defaultSummary(svmFit$pred, lev = NULL, model = NULL)
# twoClassSummary(svmFit$pred, lev = NULL, model = NULL)
# predicted <-  svmFit$pred$pred
# observed <- iris$Species
# apply(predicted, 2, postResample, obs = observed)

model <- ksvm(Species~.,data=iris,type="C-svc",kernel="rbfdot",C=tune$C,kpar=list(sigma = tune$sigma))

iris.res<-predict(model,iris)
table(iris.res,iris$Species)