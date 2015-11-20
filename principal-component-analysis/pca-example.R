# attitude : Attitudes Toward Supervisors
# [,1]	rating	numeric	Overall rating
# [,2]	complaints	numeric	Handling of employee complaints
# [,3]	privileges	numeric	Gives special privileges
# [,4]	learning	numeric	Opportunity to learn
# [,5]	raises	numeric	Gives raises
# [,6]	critical	numeric	Too critical
# [,7]	advancel	numeric	Advancement

head(attitude)
# rating complaints privileges learning raises critical advance
# 1     43         51         30       39     61       92      45
# 2     63         64         51       54     63       73      47
# 3     71         70         68       69     76       86      48
# 4     61         63         45       47     54       84      35
# 5     81         78         56       66     71       83      47
# 6     43         55         49       44     54       49      34

attitude.pc <- prcomp(attitude, scale=TRUE)

# 各主成分の標準偏差, 寄与率, 累積寄与率
summary(attitude.pc)
# Importance of components:
#                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
# Standard deviation     1.9278 1.0681 0.9204 0.78286 0.56892 0.46747 0.37475
# Proportion of Variance 0.5309 0.1630 0.1210 0.08755 0.04624 0.03122 0.02006
# Cumulative Proportion  0.5309 0.6939 0.8149 0.90248 0.94872 0.97994 1.00000

# Rotation : 各主成分の固有ベクトル (合成変数の意味)
attitude.pc$rotation
# PC1         PC2        PC3        PC4          PC5         PC6          PC7
# rating     0.4130048 -0.39692583  0.2634492 -0.2341088  0.143063817  0.41241010  0.597591161
# complaints 0.4405379 -0.33362706  0.2256118 -0.0022033 -0.278064283  0.22805897 -0.717205075
# privileges 0.3547748 -0.09575954 -0.1882432  0.8906996  0.005272287 -0.07598537  0.174304627
# learning   0.4285613 -0.04510225 -0.3252857 -0.2393794  0.697628303 -0.35282836 -0.200036529
# raises     0.4471312  0.17917304  0.0404021 -0.2428067 -0.556488662 -0.58548329  0.234335158
# critical   0.1853508  0.60263473  0.7008081  0.1493497  0.292800611  0.01154899 -0.056334214
# advance    0.3025594  0.56979573 -0.4956644 -0.1152367 -0.141732580  0.55201569 -0.004296286

# Eigenvalue
screeplot(attitude.pc)

# 主成分得点
biplot(attitude.pc, choices=c(1, 2))
