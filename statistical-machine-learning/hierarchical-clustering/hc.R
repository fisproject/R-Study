library(dplyr)
library(forcats)
library(ggplot2)
library(dendextend)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

feat <- iris[,-5]
label <- iris[,5]

# Agglomerative Hierarchical Clustering
feat_dist <- dist(feat, method = "euclidean")
hc <- hclust(feat_dist, method = "ward.D2")

dend <- hc %>%
  as.dendrogram() %>%
  color_branches(., k = 3, groupLabels = levels(label))

plot(dend, xlab = "", ylab = "", sub = "")

# cutting tree
cluster <- cutree(hc, k = 3) %>%
  as.factor() %>%
  fct_recode("setosa" = "1", "versicolor" = "2", "virginica" = "3")

# confusion matrix
table(cluster, label)
#              label
# cluster      setosa versicolor virginica
# setosa         50          0         0
# versicolor      0         50        14
# virginica       0          0        36

pca <- prcomp(feat, scale = T)

pca_hc <- data.frame(pc1 = pca$x[,1],
                     pc2 = pca$x[,2],
                     cluster = cluster,
                     label = label)

p <- ggplot(pca_hc, aes(x = pc1, y = pc2)) +
  geom_point(aes(colour = cluster, shape = label), size = 3) +
  scale_colour_hue() +
  labs(title = 'Hierarchical Clustering', x = "PC1", y = "PC2")
p