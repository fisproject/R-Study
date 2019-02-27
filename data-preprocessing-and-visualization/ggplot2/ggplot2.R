library(dplyr)
library(ggplot2)
library(ggrepel)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# Point & Hue & Buble
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                      colour = Species, size = Petal.Length)) +
  geom_point() +
  scale_colour_hue() +
  # scale_colour_gradient(low = "cyan", high = "magenta")
  labs(title = "Iris", x = "Sepal.Length", y = "Sepal.Width") +
  lims(x = c(4, 8), y = c(1, 5))
plot(p)
ggsave("img/iris-hue.png", plot = p)

# Point & Smoothing & Buble
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                      colour = Species, size = Petal.Length)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title = "Iris", x = "Sepal.Length", y = "Sepal.Width")
plot(p)
ggsave("img/iris-smooth.png", plot = p)

# Bar
p <- iris %>%
  group_by(Species) %>%
  summarise(Sepal.Width = mean(Sepal.Width)) %>%
  ggplot(aes(x = reorder(Species, -Sepal.Width), y = Sepal.Width)) +
    geom_bar(stat = "identity") +
    labs(title = "Iris", x = "Species", y = "Sepal.Width")
plot(p)
ggsave("img/iris-bar.png", plot = p)

# Hist & density
p <- ggplot(iris, aes(x = Sepal.Length, y = ..density..,
                      fill = Species, colour = Species)) +
  geom_histogram(alpha = 0.3, position = "identity", bins = 20, size = 1) +
  labs(title = "Iris", x = "Sepal.Length") +
  geom_density(alpha = 0)
plot(p)
ggsave("img/iris-hist.png", plot = p)

# Line & Smoothing & Rect
p <- ggplot(economics, aes(x = date, y = psavert)) +
  annotate("rect",
           xmin = as.Date("2008-09-15"), xmax = as.Date("2015-04-01"),
           ymin = 0, ymax = Inf, fill = "magenta", alpha = 0.1) +
  geom_line() +
  stat_smooth(color = "cyan", method = "loess") +
  labs(title = "Economics", x = "Date", y = "psavert")
plot(p)
ggsave("img/economics-line.png", plot = p)

# Stacked Bar
p <- iris %>%
  group_by(Species, Sepal.Length) %>%
  count() %>%
  ggplot(aes(x = Sepal.Length, y = n)) +
    geom_bar(stat = "identity", aes(fill = Species), alpha = 0.7) +
    scale_x_discrete() +
    lims(x = c(4, 8)) +
    labs(title = "Iris", x = "Sepal.Length", y = "count")
plot(p)
ggsave("img/iris-stacked-bar.png", plot = p)

# Tile plot
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_tile(aes(fill = Species))
plot(p)
ggsave("img/iris-tile.png", plot = p)

# Tile qplot
p <- qplot(Sepal.Length, Sepal.Width, data = iris,
           fill = Petal.Length, geom = "tile") +
  scale_fill_gradient(limits = c(0, 6), low = "white", high = "black")
plot(p)
ggsave("img/iris-tile-qplot.png", plot = p)

# Heat map
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_continuous(low = "cyan", high = "magenta") +
  geom_point(color = "black")
plot(p)
ggsave("img/iris-heatmap.png", plot = p)

# Stacked Line
iris_with_idx <- iris %>%
  mutate(index = c(c(1:50), c(1:50), c(1:50)))

p <- iris_with_idx %>%
  ggplot(aes(x = index, y = Petal.Length)) +
  geom_area(aes(group = Species, fill = Species), alpha = 0.7) +
  labs(title = "Iris", x = "Index", y = "Petal.Length")
plot(p)
ggsave("img/iris-stacked-line.png", plot = p)

# Line with label
p <- iris_with_idx %>%
  ggplot(aes(x = index, y = Petal.Length, color = Species)) +
  geom_line() +
  geom_point() +
  labs(title = "Iris", x = "Index", y = "Petal.Length") +
  geom_label_repel(data = subset(iris_with_idx, index == max(index)),
                   aes(label = Species),
                   nudge_x = 100,
                   segment.alpha = 0.9,
                   size = 3) +
  theme(legend.position = "none")
plot(p)
ggsave("img/iris-line-with-label.png", plot = p)

# Line & Point
tf <- rep(F, 150)
tf[which.min(iris$Sepal.Width)] <- T
df <- data.frame(x = c(1:150), y = iris$Sepal.Width, tf)

p <- ggplot(df, aes(x = x, y = y, color = ifelse(tf == T, "A", "B")))+
  geom_point(size = 3) +
  scale_color_manual(guide = FALSE, values = c("red", "black")) +
  geom_line(data = df, aes(x = x, y = y, color = "B")) +
  labs(title = "Iris", x = "Index", y = "Sepal.Width")
plot(p)
ggsave("img/iris-conditional-coloring.png", plot = p)

# Contours of 3d data
volcano3d <- reshape2::melt(volcano)
names(volcano3d) <- c("x", "y", "z")

p <- ggplot(volcano3d, aes(x = x, y = y, z = z)) +
  stat_contour(aes(fill = ..level..), geom = "polygon") +
  scale_fill_continuous(low = "gray", high = "black")
plot(p)
ggsave("img/volcano-contours.png", plot = p)
