require(MASS)
require(ggplot2)

data(mtcars)
d <- as.data.frame(subset(mtcars, select=c(mpg, am, vs)))
# > head(d)
#                    mpg am vs
# Mazda RX4         21.0  1  0
# Mazda RX4 Wag     21.0  1  0
# Datsun 710        22.8  1  1
# Hornet 4 Drive    21.4  0  1
# Hornet Sportabout 18.7  0  0
# Valiant           18.1  0  1

model <- glm(vs ~ mpg,
             family=binomial(link=logit),
             data=d
         )

summary(model)
# Deviance Residuals:
#     Min       1Q   Median       3Q      Max
# -2.2127  -0.5121  -0.2276   0.6402   1.6980
#
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -8.8331     3.1623  -2.793  0.00522 **
# mpg           0.4304     0.1584   2.717  0.00659 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 43.860  on 31  degrees of freedom
# Residual deviance: 25.533  on 30  degrees of freedom
# AIC: 29.533
#
# Number of Fisher Scoring iterations: 6

unknown = data.frame(mpg=c(24.0, 13.2, 18.2, 19.9))
pred <- predict(model, unknown, type="response")
probs <- data.frame(unknown, data.frame(pred))

g <- ggplot(d, aes(x=mpg, y=vs))
g +  geom_point() + stat_smooth(method="glm", family="binomial", se=FALSE) +
 layer(data=probs, mapping=aes(x=mpg, y=prob), geom="point", position="identity", colour="red")
