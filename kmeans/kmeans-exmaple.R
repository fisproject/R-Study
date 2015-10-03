require(useful)
require(ggplot2)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

wine <- read.csv("data/wine.data", header=FALSE)
colnames(wine) <- c("class","Alcohol","Malic Acid","Ash","Alcalinity of Ash","Magnesium",
                    "Total Phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins",
                    "Color Intensity","Hue","0D280/OD315 of Diluted Wines","Proline")

wine.class <- as.factor(wine[,c(1)])
wine <- scale(wine)
wine.nonlabeled <- wine[,c(-1)]

set.seed(12345)

# K-means
wine.km <- kmeans(wine.nonlabeled, centers=3)
summary(as.factor(wine.km$cluster))
table(wine.class, wine.km$cluster)
# wine.class  1  2  3
#          1 59  0  0
#          2  3  3 65
#          3  0 48  0

# PCA
pca <- prcomp(wine.nonlabeled, scale=TRUE)
pc1 <- pca$x[,1]
pc2 <- pca$x[,2]

r <- data.frame(x=pc1, y=pc2, cluster=as.factor(wine.km$cluster), label=wine.class)

g <- ggplot(
  r,
  aes(
      x = x,
      y = y,
      shape = label
  )
)

p <- g + geom_point(aes(colour=cluster), size=4) +
      labs(title='K-means', x="PC1", y="PC2") +
      scale_colour_hue()

plot(p)

# Hartigan's Method
wine.fit <- FitKMeans(wine.nonlabeled, max.clusters=20, nstart=25, seed=12345)
# Clusters  Hartigan AddCluster
# 1         2 69.523332       TRUE
# 2         3 52.151051       TRUE
# 3         4 15.207285       TRUE
# 4         5 11.604607       TRUE
# 5         6  9.896997      FALSE
# 6         7  9.771937      FALSE
# 7         8 10.969714       TRUE
# 8         9  7.562533      FALSE
# 9        10  8.752139      FALSE
# 10       11  8.474434      FALSE
# 11       12  6.283500      FALSE
# 12       13  5.780901      FALSE
# 13       14  5.707151      FALSE
# 14       15  4.512510      FALSE
# 15       16  4.813462      FALSE
# 16       17  4.508877      FALSE
# 17       18  5.790036      FALSE
# 18       19  5.384525      FALSE
# 19       20  2.358511      FALSE

PlotHartigan(wine.fit)
