# install.packages("e1071")
require(e1071)

summary(iris)

pairs(iris[1:4], main = "Iris Data (red=setosa,green=versicolor,blue=virginica)",
 	 pch = 21, bg = c("red", "green", "blue")[unclass(iris$Species)])

iris.bayes <- naiveBayes(iris[,1:4], iris[,5])

table(predict(iris.bayes, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
# result :
#             actual
# predicted    setosa versicolor virginica
#   setosa         50          0         0
#   versicolor      0         47         3
#   virginica       0          3        47
