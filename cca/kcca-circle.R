library(kernlab)
library(data.table)
library(ggplot2)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- data.frame(fread("data/circle.csv"))

x <- matrix(d$V1)
y <- matrix(d$V2)

p <- ggplot(
  d,
  aes(
    x=V1,
    y=V2
  )
)

g <- p + geom_point(alpha=1) + labs(title="circle", x="x", y="y")
plot(g)

# cca
d.cancor <- cancor(x, y)

# correlations
d.cancor$cor
# [1] 0.0111915

d.cancor$xcoef
d.cancor$ycoef

# kernel cca
d.kcca <- kcca(
  x,
  y,
  kernel="rbfdot",
  kpar=list(sigma=1),
  gamma=1,
  ncomps=1
)

# Correlation coefficients in feature space
kcor(d.kcca)
# [1] -0.9960846

p <- ggplot(
  d,
  aes(
    x=xcoef(d.kcca),
    y=ycoef(d.kcca)
  )
)

g <- p + geom_point(alpha=1) + labs(title="circle-kcca", x="f(x)", y="g(x)")
plot(g)
