library(useful)
library(ggplot2)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

set.seed(12345)

wine <- read.csv("data/wine.data", header = FALSE)
colnames(wine) <- c("class","Alcohol","Malic Acid","Ash","Alcalinity of Ash","Magnesium",
                    "Total Phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins",
                    "Color Intensity","Hue","0D280/OD315 of Diluted Wines","Proline")

wine.class <- as.factor(wine[,c(1)])
wine.index <- c(1:nrow(wine))
wine <- scale(wine)
wine.unlabeled <- wine[,c(-1)]

# K-means
wine.km <- kmeans(wine.unlabeled, centers = 3)
summary(as.factor(wine.km$cluster))
table(wine.class, wine.km$cluster)
# wine.class  1  2  3
#          1 59  0  0
#          2  3  3 65
#          3  0 48  0

# PCA
pca <- prcomp(wine.unlabeled, scale = TRUE)

summary(pca)
# Importance of components:
#                          PC1    PC2    PC3     PC4     PC5     PC6     PC7     PC8     PC9   PC10
# Standard deviation     2.169 1.5802 1.2025 0.95863 0.92370 0.80103 0.74231 0.59034 0.53748 0.5009
# Proportion of Variance 0.362 0.1921 0.1112 0.07069 0.06563 0.04936 0.04239 0.02681 0.02222 0.0193
# Cumulative Proportion  0.362 0.5541 0.6653 0.73599 0.80162 0.85098 0.89337 0.92018 0.94240 0.9617

pca$rotation
#                                PC1          PC2         PC3         PC4         PC5
# Alcohol                      -0.144329395  0.483651548 -0.20738262  0.01785630 -0.26566365
# Malic Acid                    0.245187580  0.224930935  0.08901289 -0.53689028  0.03521363
# Ash                           0.002051061  0.316068814  0.62622390  0.21417556 -0.14302547
# Alcalinity of Ash             0.239320405 -0.010590502  0.61208035 -0.06085941  0.06610294
# Magnesium                    -0.141992042  0.299634003  0.13075693  0.35179658  0.72704851
# Total Phenols                -0.394660845  0.065039512  0.14617896 -0.19806835 -0.14931841
# Flavanoids                   -0.422934297 -0.003359812  0.15068190 -0.15229479 -0.10902584
# Nonflavanoid Phenols          0.298533103  0.028779488  0.17036816  0.20330102 -0.50070298
# Proanthocyanins              -0.313429488  0.039301722  0.14945431 -0.39905653  0.13685982
# Color Intensity               0.088616705  0.529995672 -0.13730621 -0.06592568 -0.07643678
# Hue                          -0.296714564 -0.279235148  0.08522192  0.42777141 -0.17361452
# 0D280/OD315 of Diluted Wines -0.376167411 -0.164496193  0.16600459 -0.18412074 -0.10116099
# Proline                      -0.286752227  0.364902832 -0.12674592  0.23207086 -0.15786880

biplot(pca, choices = c(1, 2))

df_pca <- data.frame(
  pc1 = pca$x[,1],
  pc2 = pca$x[,2],
  cluster = as.factor(wine.km$cluster),
  label = wine.class,
  index = wine.index
)

g <- ggplot(df_pca, aes(x = pc1, y = pc2, shape = label))
p <- g + geom_point(aes(colour = cluster), size = 4) +
      labs(title = "K-means", x = "PC1", y="PC2") +
      scale_colour_hue()
plot(p)

p <- g + geom_point(aes(colour = cluster), size = 4) +
      labs(title = "K-means", x = "PC1", y = "PC2") +
      scale_colour_hue() +
      geom_text(aes(label = index), hjust = 0, vjust = 0, size = 4)
plot(p)

# Hartigan's Method
wine.fit <- FitKMeans(wine.unlabeled, max.clusters = 20, nstart = 25, seed = 12345)
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
