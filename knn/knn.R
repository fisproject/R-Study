library(class)
library(dplyr)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

wine <- read.csv("data/wine.data", header = FALSE)
colnames(wine) <- c("class", "Alcohol", "Malic Acid", "Ash", "Alcalinity of Ash",
                    "Magnesium", "Total Phenols", "Flavanoids", "Nonflavanoid Phenols",
                    "Proanthocyanins", "Color Intensity", "Hue",
                    "0D280/OD315 of Diluted Wines", "Proline")

wine$class <- as.factor(wine$class)

wine_c1 <- wine %>%
  filter(class == 1)

wine_c2 <- wine %>%
  filter(class == 2)

wine_c3 <- wine %>%
  filter(class == 3)

train_X <- rbind(wine_c1[1:30,-1], wine_c2[1:30,-1], wine_c3[1:30,-1])
train_y <- c(wine_c1$class[1:30], wine_c2$class[1:30], wine_c3$class[1:30])
test_X <- rbind(wine_c1[31:59,-1], wine_c2[31:71,-1], wine_c3[31:48,-1])
test_y <- c(wine_c1$class[31:59], wine_c2$class[31:71], wine_c3$class[31:48])

# knn
res <- knn(train_X, test = test_X, cl = train_y, k = 3, prob = TRUE)

table(res)
# res
#  1  2  3
# 25 27 23

table(test_y)
# test_y
#  1  2  3
# 25 25 25

# accuracy
sum(res == test_y)/length(test_y)
# [1] 0.92
