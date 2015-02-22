# attitude : Attitudes Toward Supervisors
# [,1]	rating	numeric	Overall rating
# [,2]	complaints	numeric	Handling of employee complaints
# [,3]	privileges	numeric	Gives special privileges
# [,4]	learning	numeric	Opportunity to learn
# [,5]	raises	numeric	Gives raises
# [,6]	critical	numeric	Too critical
# [,7]	advancel	numeric	Advancement

# correlation matrix 
cor(attitude)

attitude.pc <- prcomp(attitude, scale=TRUE)
summary(attitude.pc)
# Standard deviation (sdev) : 各主成分の標準偏差, 固有値の正の平方根
# Rotation : 各主成分の固有ベクトル
# Proportion of Variance : 寄与率
# Cumulative Proportion : 累積寄与率

# Eigenvalue
screeplot(attitude.pc)

biplot(attitude.pc, choices=c(1, 2))