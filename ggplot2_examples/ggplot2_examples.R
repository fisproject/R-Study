# install.packages("ggplot2")
library(ggplot2)

d <- iris

# gradient
p <- ggplot(d, aes(x = d$Sepal.Length, y = d$Sepal.Width)) 
p + geom_point( aes(size = d$Petal.Length, colour = d$Petal.Width), alpha = 1 ) + scale_colour_gradient(low = "cyan", high="magenta")

# hue
p <- ggplot(d, aes(x = d$Sepal.Length, y = d$Sepal.Width)) 
p + geom_point( aes(size = d$Petal.Length, colour = d$Species), alpha = 1 ) + scale_colour_hue()
