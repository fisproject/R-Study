# install.packages("rgl")
library(rgl)

air <- na.omit(airquality)
pairs(airquality[,1:4],pch=21,bg="red",cex=1)

air.lm <-lm(air$Ozone~air$Solar.R+air$Wind+air$Temp, data=air)
summary(air.lm)

step(air.lm)

plot3d(air$Wind, air$Temp, air$Ozone,
	xlab = "wind", ylab = "temp", zlab = "ozone",
	size = 1, type = "s", col = "blue")

# add predict
air.pre = predict(air.lm)
plot3d(air$Wind, air$Temp, air.pre,
	xlab = "wind", ylab = "temp", zlab = "ozone",
	size = 1, type = "s", add = TRUE, col = "red")

writeWebGL(width=500, height=550)