# install.packages("caret")
require(caret)
require(e1071)
require(kernlab)

grid <- expand.grid(.C=((2:20)*0.5), .sigma=((1:10)*0.01))
ctrl <- trainControl(method="cv", savePred=T, classProb=T, number=3)

# svmLinear : Linear kernel
svmFit <- train(Species~.,
                data=iris,
                method="svmLinear",
                trace=T,
                trControl=ctrl,
                tuneGrid=grid
          )

# eval
print(svmFit)

tune <- svmFit$bestTune
svmFit$finalModel

head(svmFit$pred)
print(svmFit$pred)

model <- ksvm(Species~.,
              data=iris,
              type="C-svc",
              kernel="vanilladot",
              C=tune$C
         )

iris.res <- predict(model, iris)
table(iris.res, iris$Species)
