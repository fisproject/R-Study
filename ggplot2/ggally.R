library(ggplot2)
library(GGally)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

p <- ggpairs(
  iris,
  columns = c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  mapping = aes(color = Species), # color needs Factor
  lower = list(continuous = "smooth", combo = "facetdensity")
)
p
