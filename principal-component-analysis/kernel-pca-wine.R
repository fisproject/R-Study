require(ggplot2)
require(data.table)
require(kernlab)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- data.frame(fread("data/wine.data"))
colnames(d) <- c(
    "class","Alcohol","Malic Acid","Ash","Alcalinity of Ash","Magnesium",
    "Total Phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins",
    "Color Intensity","Hue","0D280/OD315 of Diluted Wines","Proline"
)

# pca
d.pc <- prcomp(d, scale=TRUE)
pc <- data.frame(
    pc1=d.pc$x[,1],
    pc2=d.pc$x[,2],
    label=factor(d[,1])
)

p <- ggplot(
  pc,
  aes(
    x=pc1,
    y=pc2
  )
)

g <- p + geom_point(aes(colour=label), alpha=1) +
      labs(title="wine-pca", x="pc1", y="pc2")
plot(g)

# kernel pca
dd <- data.frame(scale(d))
d.kpc <- kpca(
  ~.,
  data=dd[,2:14],
  kernel="rbfdot",
  features=2,
  kpar=list(sigma=0.5)
)

kpc <- data.frame(
    pc1=pcv(d.kpc)[,1],
    pc2=pcv(d.kpc)[,2],
    label=factor(d[,1])
)

p <- ggplot(
  kpc,
  aes(
    x=pc1,
    y=pc2
  )
)

g <- p + geom_point(aes(colour=label), alpha=1) +
      labs(title="wine-kpca", x="pc1", y="pc2")
plot(g)
