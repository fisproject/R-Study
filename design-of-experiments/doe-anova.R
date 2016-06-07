require(DoE.base)
require(reshape2)
require(ggplot2)
require(ggfortify)

# for car::Anova
options(contrasts = c("contr.sum", "contr.sum"))

# Case I
name <- c("john", "bob", "ben")
age <- c(32 ,43 , 23)
joined <- c(2005, 1998, 2010)
group <- c("A", "B", "C")

table1 <- oa.design(
  factor.names = list(name = name, age = age, joined = joined, group = group),
  seed = 1)

data <- cbind(table1, R = c(2, 3, 12, 4, 10, 2, 10, 2, 3))

p1 <- ggplot(data, aes(x = A, y = R))
p2 <- ggplot(data, aes(x = B, y = R))
p3 <- ggplot(data, aes(x = C, y = R))
p <- new('ggmultiplot', plots = list(p1, p2, p3), ncol = 3)
p[1:3] <- p[1:3] + geom_boxplot()
p

# Type I
res.aov <- aov(R ~ name + age + joined + group, data)
summary(res.aov)

model <- lm(R ~ name + age + joined + group, data)
res.anova <- stats::anova(model)
print(res.anova)


# Case II
table2 <- oa.design(nfactors = 4, nlevels = 3, replication = 2, seed = 1)

data <- cbind(table2, R = c(5, 10, 6, 5, 8, 7, 7, 2, 3,
    6, 12, 5, 9, 9, 15, 5, 3, 13))

# Type I
res.aov <- aov(R ~ A + B + C + D, data)
summary(res.aov)
# Df Sum Sq Mean Sq F value Pr(>F)
# A            2  10.11    5.06   0.555  0.593
# B            2   3.44    1.72   0.189  0.831
# C            2 118.11   59.06   6.482  0.018 *
# D            2   3.44    1.72   0.189  0.831
# Residuals    9  82.00    9.11
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

model <- lm(R ~ A + B + C + D, data)
res.anova <- stats::anova(model)
print(res.anova)
# Analysis of Variance Table
#
# Response: R
#           Df  Sum Sq Mean Sq F value  Pr(>F)
# A          2  10.111   5.056  0.5549 0.59260
# B          2   3.444   1.722  0.1890 0.83097
# C          2 118.111  59.056  6.4817 0.01805 *
# D          2   3.444   1.722  0.1890 0.83097
# Residuals  9  82.000   9.111
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Type II
model2 <- lm(R ~ A * B * C * D, data = data)
res.car.anova <- car::Anova(model2, Type = 2)
print(res.car.anova)
# Anova Table (Type II tests)
#
# Response: R
#            Sum Sq Df F value  Pr(>F)
# A          10.111  2  0.5549 0.59260
# B           3.444  2  0.1890 0.83097
# C         118.111  2  6.4817 0.01805 *
# D           3.444  2  0.1890 0.83097
# A:B                0
# A:C                0
# B:C                0
# A:D                0
# B:D                0
# C:D                0
# A:B:C              0
# A:B:D              0
# A:C:D              0
# B:C:D              0
# A:B:C:D            0
# Residuals  82.000  9
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
