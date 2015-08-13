require(fastICA)
require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# load bayeslingam : http://www.cs.helsinki.fi/group/neuroinf/lingam/bayeslingam/
source('./../../trunk/main/loud.R')
loud()

set.seed(123)

# ex1) x1→x2
X1 <- runif(10000, -sqrt(3), sqrt(3))
X2 <- sqrt(2/3) * X1 + runif(10000,-1,1)

summary(X1)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -1.732000 -0.854100 -0.018250 -0.008376  0.842700  1.732000
summary(X2)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -2.38200 -0.73510 -0.01060 -0.01439  0.69710  2.39400
var(X1)
# [1] 0.9863195
var(X2)
# [1] 0.97352

d <- data.frame(x1=X1, x2=X2)
p <- ggplot(d,
      aes(
        x=x1,
        y=x2
      )
    )
p <- p + geom_point() + labs(title="", x="x1", y="x2")
plot(p)
ggsave("./img/point-ex1.png", p)

d.res <- bayeslingam(d)
d.prob <- data.frame(index=c("x1 x2","x1->x2","x1<-x2"), prob=d.res$prob)

p <- ggplot(d.prob,
      aes(
        x=index,
        y=prob
      )
    )
p <- p + geom_bar(width=0.5, stat="identity") + labs(title="", x="index", y="prob")
plot(p)
ggsave("./img/prob-ex1.png", p)


# ex2) x2→x1
X2 <- runif(10000, -sqrt(3), sqrt(3))
X1 <- sqrt(2/3) * X2 + runif(10000,-1,1)

summary(X2)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -1.731000 -0.860400 -0.005832  0.001931  0.883400  1.732000
summary(X1)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -2.394000 -0.736300 -0.003375 -0.012690  0.709600  2.397000
var(X1)
# [1] 0.9844444

var(X2)
# [1] 1.008017

dd <- data.frame(x1=X1, x2=X2)
p <- ggplot(dd,
      aes(
        x=x1,
        y=x2
      )
    )
p <- p + geom_point() + labs(title="", x="x1", y="x2")
plot(p)
ggsave("./img/point-ex2.png", p)

dd.res <- bayeslingam(dd)
dd.prob <- data.frame(index=c("x1 x2","x1->x2","x1<-x2"), prob=dd.res$prob)

p <- ggplot(dd.prob,
      aes(
        x=index,
        y=prob
      )
    )
p <- p + geom_bar(width=0.5, stat="identity") + labs(title="", x="index", y="prob")
plot(p)
ggsave("./img/prob-ex2.png", p)


# ex3) x1 x2
X1 <- runif(10000, -sqrt(3), sqrt(3))
X2 <- runif(10000, -sqrt(3), sqrt(3))

summary(X1)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
# -1.7320000 -0.8744000 -0.0019380 -0.0007932  0.8674000  1.7320000
summary(X2)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -1.731000 -0.850800  0.001823  0.007475  0.883000  1.731000
var(X1)
# [1] 1.001325
var(X2)
# [1] 1.007101

d <- data.frame(x1=X1, x2=X2)
p <- ggplot(d,
      aes(
        x=x1,
        y=x2
      )
    )
p <- p + geom_point() + labs(title="", x="x1", y="x2")
plot(p)
ggsave("./img/point-ex3.png", p)

d.res <- bayeslingam(d)
d.prob <- data.frame(index=c("x1 x2","x1->x2","x1<-x2"), prob=d.res$prob)

p <- ggplot(d.prob,
      aes(
        x=index,
        y=prob
      )
    )
p <- p + geom_bar(width=0.5, stat="identity") + labs(title="", x="index", y="prob")
plot(p)
ggsave("./img/prob-ex3.png", p)


# ex4) x1->x2
X1 <- rnorm(10000, 0, 1)
X2 <- 0.05 * X1 + rnorm(10000, 0, 1)

summary(X1)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -4.129000 -0.679300  0.002828 -0.005245  0.667400  3.983000
summary(X2)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -3.54500 -0.65900  0.01868  0.01484  0.68920  3.61000
var(X1)
# [1] 0.9983561
var(X2)
# [1] 1.005119

d <- data.frame(x1=X1, x2=X2)
p <- ggplot(d,
      aes(
        x=x1,
        y=x2
      )
    )
p <- p + geom_point() + labs(title="", x="x1", y="x2")
plot(p)
ggsave("./img/point-ex4.png", p)

d.res <- bayeslingam(d)
d.prob <- data.frame(index=c("x1 x2","x1->x2","x1<-x2"), prob=d.res$prob)

p <- ggplot(d.prob,
      aes(
        x=index,
        y=prob
      )
    )
p <- p + geom_bar(width=0.5, stat="identity") + labs(title="", x="index", y="prob")
plot(p)
ggsave("./img/prob-ex4.png", p)
