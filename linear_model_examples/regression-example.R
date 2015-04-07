# install.packages("ppcor")
# install.packages("car")
# install.packages("rgl")
require(ppcor)
require(car)
require(rgl)

# airquality : Daily air quality measurements in New York. May to September 1973.
air <- na.omit(airquality)
pairs(airquality[, 1:4], pch=21, bg="red", cex=1)
pcor(air)
# $estimate
#              Ozone     Solar.R        Wind        Temp       Month         Day
# Ozone    1.0000000  0.20505146 -0.44897864  0.55975584 -0.19236023  0.11559389
# Solar.R  0.2050515  1.00000000  0.11427512  0.14942920 -0.17190775 -0.04728812
# Wind    -0.4489786  0.11427512  1.00000000 -0.07988093 -0.07467615  0.05413314
# Temp     0.5597558  0.14942920 -0.07988093  1.00000000  0.43771581 -0.12748877
# Month   -0.1923602 -0.17190775 -0.07467615  0.43771581  1.00000000  0.04833785
# Day      0.1155939 -0.04728812  0.05413314 -0.12748877  0.04833785  1.00000000

air.lm <- lm(air$Ozone ~ air$Solar.R + air$Wind + air$Temp, data=air)
vif(air.lm)
# air$Solar.R    air$Wind    air$Temp
#    1.095253    1.329070    1.431367

summary(air.lm)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) -64.34208   23.05472  -2.791  0.00623 **
# air$Solar.R   0.05982    0.02319   2.580  0.01124 *
# air$Wind     -3.33359    0.65441  -5.094 1.52e-06 ***
# air$Temp      1.65209    0.25353   6.516 2.42e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

step(air.lm)

plot3d(air$Wind, air$Temp, air$Ozone,
	xlab = "wind", ylab = "temp", zlab = "ozone",
	size = 1, type = "s", col = "blue")

# Add plot
air.pre = predict(air.lm)
plot3d(air$Wind, air$Temp, air.pre,
	xlab = "wind", ylab = "temp", zlab = "ozone",
	size = 1, type = "s", add = TRUE, col = "red")

grid3d(c("x", "y+", "z"))

writeWebGL(width=500, height=550)
