require(ggplot2)
require(reshape2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

base1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))
base2 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species))
base3 <- ggplot(
    iris,
    aes(x = Sepal.Length, y = ..density.., fill = Species, colour = Species)
)

# Hue
g <- base2 + geom_point() +
    labs(title="Iris", x="Sepal.Length", y="Sepal.Width") +
    scale_colour_hue()
    # scale_colour_gradient(low="cyan", high="magenta")
plot(g)
ggsave("img/iris-hue.png", g)

# Smoothing
g <- base2 + geom_point() +
    labs(title="Iris", x="Sepal.Length", y="Sepal.Width") +
    stat_smooth(method="lm")
plot(g)
ggsave("img/iris-smooth.png", g)

# Hist & density
g <- base3 + geom_histogram(alpha=0.3, position="identity", size=1) +
      labs(title="Iris", x="Sepal.Length") +
      geom_density(alpha=0)
plot(g)
ggsave("img/iris-hist.png", g)

# Tile
g <- base1 + geom_tile(aes(fill=Species))
plot(g)
ggsave("img/iris-tile.png", g)

# Tile qplot
g <- qplot(Sepal.Length, Sepal.Width, fill=Petal.Length, data=iris, geom="tile") +
    scale_fill_gradient(limits=c(0, 6), low="white", high="black")
plot(g)
ggsave("img/iris-tile-qplot.png", g)

# Heat map
g <- base1 + stat_density2d(aes(fill=..level..), geom="polygon") +
    scale_fill_continuous(low = "cyan", high = "magenta") +
    geom_point(color = "black")
plot(g)
ggsave("img/iris-heatmap.png", g)

# Contours of 3d data
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
base4 <- ggplot(volcano3d, aes(x = x, y = y, z = z))

g <- base4 + stat_contour(aes(fill=..level..), geom="polygon") +
    scale_fill_continuous(low = "gray", high = "black")
plot(g)
ggsave("img/volcano-contours.png", g)
