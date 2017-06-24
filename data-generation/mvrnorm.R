require(MASS)
require(ggplot2)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

set.seed(123)

genelate <- function(dims) {
  mu <- rep(0, dims)
  sigma <- diag(dims)
  mgauss <- mvrnorm(1000, mu, sigma)

  # save as csv  
  fname <- paste(paste("./data/", dims, sep = ""), "d-gauss.csv", sep = "")
  write.csv(df, fname, row.names = FALSE)

  # plot
  df <- data.frame(mgauss)
  p <- ggplot() + geom_point(data = df, aes(x = X1, y = X2)) +
    labs(title = fname, x = "X1", y = "X2")
  plot(p)
  
  return(NULL)
}

lapply(list(2,3,4,5), genelate)