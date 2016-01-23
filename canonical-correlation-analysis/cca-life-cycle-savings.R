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

d.cancor <- cancor(x, y)

# correlations
d.cancor$cor
# [1] 0.8247966 0.3652762

d.cancor$xcoef
#         [,1]       [,2]
# pop15 -0.08338007 -0.3314944
# pop75  0.06279282 -0.3360027

d.cancor$ycoef
#         [,1]        [,2]         [,3]
# sr   0.03795363  0.14955310 -0.023106040
# dpi  0.12954600 -0.07518943  0.004502216
# ddpi 0.01196908 -0.03520728  0.148898175
