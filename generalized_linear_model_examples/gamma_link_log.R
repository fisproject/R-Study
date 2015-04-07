# http://www.stat.sc.edu/~hitchcock/stat599chap7Rcode.txt

require(faraway)

# 半導体ウェハの抵抗値測定 resitivity of wafer in semiconductor experiment
data(wafer)
attach(wafer)

wafer.lm <- lm (log(resist) ~ (x1+x2+x3+x4)^2, data=wafer)

step.lognorm.wafer <- step(wafer.lm)
summary(step.lognorm.wafer)

# Fitting the same model but with a gamma GLM with a log link:
wafer.glm <- glm(resist ~ (x1+x2+x3+x4)^2, family=Gamma(link=log), data=wafer)

# "link=log" must be specified, since the canonical link
step.gamma.wafer <- step(wafer.glm)
summary(step.gamma.wafer)

plot(resist, exp(step.lognorm.wafer$fitted.values), ylim=c(150, 350), col="blue", cex=1)
par(new=T)
plot(resist, step.gamma.wafer$fitted.values, ylim=c(150, 350), col="red", cex=0.5)
