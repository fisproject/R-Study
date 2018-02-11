library(rgl)
library(ggplot2)
library(data.table)
library(kernlab)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- data.frame(fread("data/circle-xor.csv"))

p <- ggplot(d, aes(x = V1, y = V2))
g <- p + geom_point(aes(colour = factor(V3)), alpha = 1) +
      labs(title = "circle-xor", x = "x1", y = "x2")
plot(g)

# kernel pca
dd <- data.frame(scale(d))

d.kcp <- kpca(~.,
              data = dd[,1:2],
              kernel = "rbfdot",
              features = 3,
              kpar = list(sigma = 5))

plot3d(pcv(d.kcp)[,1], pcv(d.kcp)[,2], pcv(d.kcp)[,3],
    col = rainbow(2)[factor(d[,3])],
    main = "circle-xor-kpca", xlab = "pc1", ylab = "pc2", zlab = "pc3")
