library(kernlab)
library(ggplot2)

d <- LifeCycleSavings
# A data frame with 50 observations on 5 variables.
#
# [,1]	sr	numeric	aggregate personal savings
# [,2]	pop15	numeric	% of population under 15
# [,3]	pop75	numeric	% of population over 75
# [,4]	dpi	numeric	real per-capita disposable income
# [,5]	ddpi	numeric	% growth rate of dpi

head(d)
#             sr pop15 pop75     dpi ddpi
# Australia 11.43 29.35  2.87 2329.68 2.87
# Austria   12.07 23.32  4.41 1507.99 3.93
# Belgium   13.17 23.80  4.43 2108.47 3.82
# Bolivia    5.75 41.89  1.67  189.13 0.22
# Brazil    12.88 42.19  0.83  728.47 4.56
# Canada     8.79 31.72  2.85 2982.88 2.43

x <- scale(d[,2:3])
y <- scale(d[,-c(2,3)])

p <- ggplot(
  d,
  aes(
    x=x[,1],
    y=y[,1]
  )
)

g <- p + geom_point(alpha=1) +
      labs(title="LifeCycleSavings", x="pop15", y="sr")
plot(g)

d.kcca <- kcca(
  x,
  y,
  kernel="rbfdot",
  kpar=list(sigma=0.5),
  gamma=1,
  ncomps=2
)

# Correlation coefficients in feature space
kcor(d.kcca)
# [1] -0.9898352  0.9898352

p <- ggplot(
  d,
  aes(
    x=xcoef(d.kcca)[,2],
    y=ycoef(d.kcca)[,2]
  )
)

g <- p + geom_point(alpha=1) +
    labs(title="LifeCycleSavings-kcca", x="f(x)", y="g(x)")
plot(g)
