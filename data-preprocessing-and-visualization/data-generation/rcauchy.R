library(ggplot2)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

set.seed(12345)

n <- 1000
df <- data.frame(
  X1 = rcauchy(n, location = 0, scale = 1),
  X2 = rcauchy(n, location = 0, scale = 1)
)

# save as csv
fname <- "./data/2d-cauchy.csv"
write.csv(df, fname, row.names = FALSE)

# plot
p <- ggplot() + geom_point(data = df, aes(x = X1, y = X2)) +
  labs(title = fname, x = "X1", y = "X2")
plot(p)
