# install.packages("caret")
library(caret)
library(e1071)
library(kernlab)
library(ggplot2)

# Color Define
cCyan <- "#00a0e9"
cMagenta <- "#e4007f"
cGreen <- "#009944"
cOrange <- "#f39800"
cLightBlue <- "#0068b7"
qcolours <- c(cCyan,cMagenta,cGreen,cOrange,cLightBlue)

grid <- expand.grid(.C =((2:10)*0.5),.sigma = ((1:10)*0.01))
ctrl <- trainControl(method = "cv", savePred=T, classProb=T, number=3)

# svmRadial : gaussian kernel
svmFit<-train(Species~.,data=iris,method="svmRadial",trace=T, trControl = ctrl , tuneGrid = grid)

# eval
print(svmFit)

tune <- svmFit$bestTune
svmFit$finalModel

head(svmFit$pred)

post <- postResample(svmFit$pred$pred, svmFit$pred$obs)
R2(svmFit$pred$pred, svmFit$pred$obs, formula = "traditional", na.rm = FALSE)

# results
res.C <- svmFit$results$C
res.sigma <- svmFit$results$sigma
res.acc <- svmFit$results$Accuracy
# ggplot
p <- ggplot(svmFit$pred,aes(x=res.C,y=res.sigma))
p + geom_point(aes(colour = res.acc), size = 5) + scale_colour_gradient(low=cLightBlue,high=cOrange)

model <- ksvm(Species~.,data=iris,type="C-svc",kernel="rbfdot",C=tune$C,kpar=list(sigma = tune$sigma))

iris.res <- predict(model,iris)
table(iris.res,iris$Species)