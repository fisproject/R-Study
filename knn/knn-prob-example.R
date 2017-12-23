library(class)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# wine <- data(wine)
wine <- read.csv("data/wine.data", header=FALSE)
colnames(wine) <- c("class","Alcohol","Malic Acid","Ash","Alcalinity of Ash","Magnesium",
                    "Total Phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins",
                    "Color Intensity","Hue","0D280/OD315 of Diluted Wines","Proline")

# train
train <- rbind(wine[1:30,2:14], wine[60:90,2:14], wine[131:161,2:14])
train.label <- factor(c(wine[1:30,1], wine[60:90,1], wine[131:161,1]))

# test
test <- rbind(wine[31:59,2:14], wine[61:130,2:14], wine[161:178,2:14])
test.label <- factor(c(wine[31:59,1], wine[61:130,1], wine[161:178,1]))

# knn
res <- knn(train,test,train.label,k=3,prob=TRUE)
attributes(res)

# accuracy
sum(res==test.label)/length(test.label)
