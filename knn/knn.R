library(class)
library(dplyr)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

set.seed(123)

# wine <- data(wine)
wine <- read.csv("data/wine.data", header = FALSE)
colnames(wine) <- c("class", "Alcohol", "Malic Acid", "Ash", "Alcalinity of Ash",
                    "Magnesium", "Total Phenols", "Flavanoids", "Nonflavanoid Phenols",
                    "Proanthocyanins", "Color Intensity", "Hue",
                    "0D280/OD315 of Diluted Wines", "Proline")

wine$class <- as.factor(wine$class)

c1 <- wine %>%
  filter(class == 1)

c2 <- wine %>%
  filter(class == 2)

c3 <- wine %>%
  filter(class == 3)

t_size <- 30
train_X <- rbind(c1[1:t_size,-1], c2[1:t_size,-1], c3[1:t_size,-1])
train_y <- c(c1$class[1:t_size], c2$class[1:t_size], c3$class[1:t_size])
test_X <- rbind(c1[(t_size+1):nrow(c1),-1], c2[(t_size+1):nrow(c2),-1], c3[(t_size+1):nrow(c3),-1])
test_y <- c(c1$class[(t_size+1):nrow(c1)], c2$class[(t_size+1):nrow(c2)], c3$class[(t_size+1):nrow(c3)])

# knn
res <- knn(train_X, test = test_X, cl = train_y, k = 3, prob = TRUE)

table(res)
# res
#  1  2  3
# 28 43 17

table(test_y)
# test_y
#  1  2  3
# 29 41 18

# accuracy
sum(res == test_y)/length(test_y)
# [1] 0.7045455
