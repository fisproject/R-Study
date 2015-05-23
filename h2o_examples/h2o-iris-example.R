# install.packages("h2o")
require(h2o)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

h <- h2o.init(ip="localhost", port=54321, startH2O=TRUE, nthreads=-1)

iris = h2o.importFile(h, path="iris.csv", key="iris.hex")
len <- nrow(iris)/2 # len is 75

head(iris)
#    C1  C2  C3  C4     C5
# 1 5.1 3.5 1.4 0.2 setosa
# 2 4.9 3.0 1.4 0.2 setosa
# 3 4.7 3.2 1.3 0.2 setosa
# 4 4.6 3.1 1.5 0.2 setosa
# 5 5.0 3.6 1.4 0.2 setosa
# 6 5.4 3.9 1.7 0.4 setosa
summary(iris)

# empty vector for result
res.err.dl<-rep(0, len)

# k-fold
num <- sample(nrow(iris), nrow(iris))

for(i in 1:len){
  iris.train <- iris[-num[i],]
  iris.test <- iris[num[i],]

  # Deep Learning
  model <- h2o.deeplearning(
     x=1:4,
     y=5,
     data=iris.train,
     activation="Tanh",
     hidden=rep(20,2)
   )
  pred.dl <- h2o.predict(model, newdata=iris.test[,-5])

  pred.dl.df <- as.data.frame(pred.dl)
  test.dl.df <- as.data.frame(iris.test)

  # correct is 0, false is 1
  res.err.dl[i] <- ifelse(as.character(pred.dl.df[1,1]) == as.character(test.dl.df[1,5]), 0, 1)
}

# Error rate
sum(res.err.dl/len)
# [1] 0.08

h2o.shutdown(h)
