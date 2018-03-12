library(ggplot2)
library(kernlab)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

wine <- read.csv("data/wine.data")
colnames(wine) <- c("class", "Alcohol", "Malic Acid", "Ash", "Alcalinity of Ash",
                    "Magnesium", "Total Phenols", "Flavanoids",
                    "Nonflavanoid Phenols", "Proanthocyanins", "Color Intensity",
                    "Hue", "0D280/OD315 of Diluted Wines", "Proline")

# pca
wine_pc <- prcomp(wine, scale = TRUE)
pc <- data.frame(pc1 = wine_pc$x[,1],
                 pc2 = wine_pc$x[,2],
                 label = factor(wine$class))

p <- ggplot(pc, aes(x = pc1, y = pc2)) +
  geom_point(aes(colour = label), alpha = 1) +
  labs(title = "wine-pca", x = "pc1", y = "pc2")
plot(g)

# the proportion of the total variation
wine_scaled <- data.frame(scale(wine))
wine_cr <- princomp(wine_scaled)
screeplot(wine_cr)

# kernel pca
d.kpc <- kpca( ~ .,
              data = wine_scaled[,-1],
              kernel = "rbfdot",
              features = 2,
              kpar = list(sigma = 0.5))

kpc <- data.frame(pc1 = pcv(d.kpc)[,1],
                  pc2 = pcv(d.kpc)[,2],
                  label = factor(wine_scaled$class))

g <- ggplot(kpc, aes(x = pc1, y = pc2)) +
  geom_point(aes(colour = label), alpha = 1) +
  labs(title = "wine-kpca", x = "pc1", y = "pc2")
plot(g)
