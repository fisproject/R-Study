library(rgl)
library(ggplot2)
library(data.table)
library(kernlab)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- data.frame(fread("data/circle.csv"))

p <- ggplot(d, aes(x = V1, y = V2))

g <- p + geom_point(aes(colour = factor(V3)), alpha = 1) +
      labs(title = "circle", x = "x1", y = "x2")
plot(g)

# nonlinear conversion
answer <- data.frame(x1 = d$V1**2,
                     x2 = d$V2**2,
                     x3 = sqrt(2)*d$V1*d$V2,
                     label = d$V3)

plot3d(answer$x1, answer$x2, answer$x3,
    col = rainbow(2)[factor(answer$label)],
    main = "circle-nonlinear-conversion", xlab = "z1", ylab = "z2", zlab = "z3")

# kernel pca
dd <- data.frame(scale(d))
d.kcpa <- kpca(~.,
               data = dd[,1:2],
               kernel = "rbfdot",
               features = 3,
               kpar = list(sigma = 3))

plot3d(pcv(d.kcpa)[,1], pcv(d.kcpa)[,2], pcv(d.kcpa)[,3],
    col = rainbow(2)[factor(d[,3])],
    main = "circle-kpca", xlab = "pc1", ylab = "pc2", zlab = "pc3")
