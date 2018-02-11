library(ggplot2)
# attitude : Attitudes Toward Supervisors
# Y	    rating	numeric	Overall rating
# X[1]	complaints	numeric	Handling of employee complaints
# X[2]	privileges	numeric	Does not allow special privileges
# X[3]	learning	numeric	Opportunity to learn
# X[4]	raises	numeric	Raises based on performance
# X[5]	critical	numeric	Too critical
# X[6]	advancel	numeric	Advancement

d <- attitude
head(d)
#      rating complaints privileges learning raises critical advance
# 1     43         51         30       39     61       92      45
# 2     63         64         51       54     63       73      47
# 3     71         70         68       69     76       86      48
# 4     61         63         45       47     54       84      35
# 5     81         78         56       66     71       83      47
# 6     43         55         49       44     54       49      34

attitude.pc <- prcomp(d[,2:6], scale = TRUE)

# 各主成分の標準偏差, 寄与率, 累積寄与率
summary(attitude.pc)
# Importance of components:
#                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
# Standard deviation     1.9278 1.0681 0.9204 0.78286 0.56892 0.46747 0.37475
# Proportion of Variance 0.5309 0.1630 0.1210 0.08755 0.04624 0.03122 0.02006
# Cumulative Proportion  0.5309 0.6939 0.8149 0.90248 0.94872 0.97994 1.00000

# Rotation : 各主成分の固有ベクトル (合成変数の意味)
attitude.pc$rotation
# PC1        PC2         PC3        PC4        PC5
# complaints 0.5090589 -0.1415816  0.04991461  0.7029582  0.4734681
# privileges 0.4352465 -0.2315203 -0.83038342 -0.1716099 -0.1948647
# learning   0.4843209 -0.2397377  0.37479163 -0.6645630  0.3547484
# raises     0.5150975  0.1559771  0.37358608  0.1328045 -0.7437352
# critical   0.2269964  0.9189940 -0.16714239 -0.1308392  0.2426256

# Eigenvalue
screeplot(attitude.pc)

# 主成分得点
biplot(attitude.pc, choices = c(1, 2))

pc <- data.frame(pc1 = attitude.pc$x[,1],
                 pc2 = attitude.pc$x[,2],
                 label = d[,1])

p <- ggplot(pc, aes(x = pc1, y = pc2))
g <- p + geom_point(aes(colour = label), alpha = 1) +
      labs(title = "attitude-pca", x = "pc1", y = "pc2")
plot(g)
