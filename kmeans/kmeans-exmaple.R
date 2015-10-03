require(stats)
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

r <- data.frame(x=pc1, y=pc2, ctg=as.factor(wine.km$cluster), label=wine.class)

g <- ggplot(
  r,
  aes(
      x = x,
      y = y,
      label = label
  )
)

p <- g + geom_point(aes(colour=ctg), size=5) +
      labs(title='K-means', x="PC1", y="PC2") +
      scale_colour_hue() +
      geom_text(family="HiraMaruProN-W4", size=3)

plot(p)
