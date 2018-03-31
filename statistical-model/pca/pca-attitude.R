library(ggplot2)

# attitude : Attitudes Toward Supervisors
# Y	    rating	numeric	Overall rating
# X[1]	complaints	numeric	Handling of employee complaints
# X[2]	privileges	numeric	Does not allow special privileges
# X[3]	learning	numeric	Opportunity to learn
# X[4]	raises	numeric	Raises based on performance
# X[5]	critical	numeric	Too critical
# X[6]	advancel	numeric	Advancement

tibble::glimpse(attitude)
# Observations: 30
# Variables: 7
# $ rating     <dbl> 43, 63, 71, 61, 81, 43, 58, 71, 72, 67, 64, 67, 69, 68, 77, 8...
# $ complaints <dbl> 51, 64, 70, 63, 78, 55, 67, 75, 82, 61, 53, 60, 62, 83, 77, 9...
# $ privileges <dbl> 30, 51, 68, 45, 56, 49, 42, 50, 72, 45, 53, 47, 57, 83, 54, 5...
# $ learning   <dbl> 39, 54, 69, 47, 66, 44, 56, 55, 67, 47, 58, 39, 42, 45, 72, 7...
# $ raises     <dbl> 61, 63, 76, 54, 71, 54, 66, 70, 71, 62, 58, 59, 55, 59, 79, 6...
# $ critical   <dbl> 92, 73, 86, 84, 83, 49, 68, 66, 83, 80, 67, 74, 63, 77, 77, 5...
# $ advance    <dbl> 45, 47, 48, 35, 47, 34, 35, 41, 31, 41, 34, 41, 25, 35, 46, 3...

attitude_pc <- prcomp(attitude[,-1], scale = TRUE)

# 各主成分の標準偏差, 寄与率, 累積寄与率
summary(attitude_pc)
# Importance of components:
#                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
# Standard deviation     1.9278 1.0681 0.9204 0.78286 0.56892 0.46747 0.37475
# Proportion of Variance 0.5309 0.1630 0.1210 0.08755 0.04624 0.03122 0.02006
# Cumulative Proportion  0.5309 0.6939 0.8149 0.90248 0.94872 0.97994 1.00000

# Rotation : 各主成分の固有ベクトル (合成変数の意味)
attitude_pc$rotation
# PC1        PC2         PC3        PC4        PC5
# complaints 0.5090589 -0.1415816  0.04991461  0.7029582  0.4734681
# privileges 0.4352465 -0.2315203 -0.83038342 -0.1716099 -0.1948647
# learning   0.4843209 -0.2397377  0.37479163 -0.6645630  0.3547484
# raises     0.5150975  0.1559771  0.37358608  0.1328045 -0.7437352
# critical   0.2269964  0.9189940 -0.16714239 -0.1308392  0.2426256

# the proportion of the total variation
screeplot(attitude_pc)

# 主成分得点
biplot(attitude_pc, choices = c(1, 2))

pc <- data.frame(pc1 = attitude_pc$x[,1],
                 pc2 = attitude_pc$x[,2],
                 label = attitude$rating)

g <- ggplot(pc, aes(x = pc1, y = pc2)) +
  geom_point(aes(colour = label), alpha = 1) +
  labs(title = "attitude-pca", x = "pc1", y = "pc2")
plot(g)
