data(lalonde)
d <- lalonde
d$id <- 1:nrow(d)

model.glm <- glm(treat ~ ., data=lalonde[,-9], family=binomial)
summary(model.glm)

# caliper matching
mout <- Match(
    Y=lalonde$re78, # result
    Tr=lalonde$treat, # treat
    X=model.glm$fitted, # propensity score
    caliper=T
  )

summary(mout)
# Estimate...  2138.6
# AI SE......  797.76
# T-stat.....  2.6807
# p.val......  0.0073468
#
# Original number of observations..............  445
# Original number of treated obs...............  185
# Matched number of observations...............  185
# Matched number of observations  (unweighted).  322
#
# Caliper (SDs)........................................   TRUE
# Number of obs dropped by 'exact' or 'caliper'  0

# Inverse Probability Weighting Estimator
ivec1 <- lalonde$treat
ivec2 <- rep(1, nrow(lalonde)) - ivec1

ivec <- cbind(ivec1, ivec2)

iestp1 <- (ivec1/model.glm$fitted) * (length(ivec1)/sum(ivec1))
iestp2 <- (ivec2/(1-model.glm$fitted)) * (length(ivec2)/sum(ivec2))

iestp <- iestp1 + iestp2
head(iestp)
 #        1         2         3         4         5         6
 # 6.124465 10.588840  4.526984  7.320260  6.053291  6.745951

ipwe <- lm(lalonde$re78 ~ ivec - 1, weight=iestp, data=lalonde)
summary(ipwe)
# Weighted Residuals:
#    Min     1Q Median     3Q    Max
# -17241  -7932  -3538   5383 129799
#
# Coefficients:
#           Estimate Std. Error t value Pr(>|t|)
# ivecivec1     6213        435  14.284   <2e-16 ***
# ivecivec2     4571        514   8.894   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 14200 on 443 degrees of freedom
# Multiple R-squared:  0.3899,	Adjusted R-squared:  0.3872
# F-statistic: 141.6 on 2 and 443 DF,  p-value: < 2.2e-16
