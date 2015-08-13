# install.packages("ggplot2")
require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- iris

p <- ggplot(
  d,
  aes(
      x = d$Sepal.Length,
      y = d$Sepal.Width
  )
)

# gradient
g <- p + geom_point( aes(size = d$Petal.Length, colour = d$Petal.Width), alpha = 1 ) +
      labs(title="Iris", x="Length", y="Width") +
      scale_colour_gradient(low = "cyan", high="magenta")
plot(g)

# hue
g <- p + geom_point( aes(size = d$Petal.Length, colour = d$Species), alpha = 1 ) +
      labs(title="Iris", x="Length", y="Width") +
      scale_colour_hue()
plot(g)

# smooth lm
g <- p + geom_point(colour = "magenta") +
      labs(title="Iris", x="Length", y="Width") +
      stat_smooth(method = "lm")
plot(g)

# hist & density
p <- ggplot(
  d,
  aes(
      x = d$Sepal.Length,
      y = ..density..,
      fill = d$Species,
      colour = d$Species
  )
)
g <- p + geom_histogram(alpha = 0.3, position = "identity", size = 1) +
      labs(title="Iris", x="Length") +
      geom_density(alpha = 0)
plot(g)
ggsave("iris-hist.png", g)
