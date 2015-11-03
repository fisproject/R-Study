require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- iris

p <- ggplot(
  d,
  aes(
      x = Sepal.Length,
      y = Sepal.Width
  )
)

# Gradient
g <- p + geom_point(aes(size=Petal.Length, colour=Petal.Width), alpha=1) +
      labs(title="Iris", x="Length", y="Width") +
      scale_colour_gradient(low="cyan", high="magenta")
plot(g)
ggsave("img/iris-gradient.png", g)

# Hue
g <- p + geom_point(aes(size=Petal.Length, colour=Species), alpha=1) +
      labs(title="Iris", x="Length", y="Width") +
      scale_colour_hue()
plot(g)
ggsave("img/iris-hue.png", g)

# Smoothing
g <- p + geom_point(colour="magenta") +
      labs(title="Iris", x="Length", y="Width") +
      stat_smooth(method="lm")
plot(g)
ggsave("img/iris-smooth.png", g)

# Hist & density
p <- ggplot(
  d,
  aes(
      x = Sepal.Length,
      y = ..density..,
      fill = Species,
      colour = Species
  )
)

g <- p + geom_histogram(alpha=0.3, position="identity", size=1) +
      labs(title="Iris", x="Length") +
      geom_density(alpha=0)
plot(g)
ggsave("img/iris-hist.png", g)

# Tile
p <- ggplot(
  d,
  aes(
    x = Sepal.Length,
    y = Sepal.Width,
  )
)

g <- p + geom_tile(aes(fill=Species))
plot(g)
ggsave("img/iris-tile.png", g)

# Tile
g <- qplot(round(Sepal.Length), round(Sepal.Width), fill=Petal.Length, data=d, geom="tile") +
      scale_fill_gradient(limits=c(0, 6), low="white", high="black")
plot(g)
ggsave("img/iris-tile-qplot.png", g)
