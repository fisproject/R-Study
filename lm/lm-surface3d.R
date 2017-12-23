library(rgl)

# airquality : Daily air quality measurements in New York. May to September 1973.
air <- na.omit(airquality)
x1 <- air$Wind
x2 <- air$Temp

air.lm <-lm(Ozone ~ (x1+x2)^2, data = air)
summary(air.lm)

sapply(air, range)
#       Ozone Solar.R Wind Temp Month Day
# [1,]     1       7  2.3   57     5   1
# [2,]   168     334 20.7   97     9  31

x1 <- seq(from = 2.3, to = 20.7, by = 0.1)
x2 <- seq(from = 57, to = 97, by = 1)

air.sur <- outer(
	X = x1, Y = x2,
	FUN = function(x1, x2) {predict(air.lm, newdata = data.frame(x1 = x1, x2 = x2))}
)

plot3d(x1, x2, air$Ozone, xlab = "wind", ylab = "temp", zlab = "ozone",
	size = 1, type = "s", col = "blue")

surface3d(x = x1, y = x2, z = air.sur,
	col = "red", alpha = 0.5)

writeWebGL(width = 500, height = 550)
