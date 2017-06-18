require(kernlab)

featureMat <- as.matrix( iris[1:100, c(4,3)] )
labelVec <- as.integer(iris$Species[1:100])
labelVec[labelVec==2] <- -1

# 制約式
X <- cbind(1,featureMat)
Y <- diag(labelVec)
A <- Y %*% X
b <- rep( 1, length = dim(X)[1] )
r <- rep( 5000, length = dim(X)[1] )
u <- rep( 5000, length = dim(X)[2] )
l <- -u

# 目的関数
H <- diag(dim(X)[2])
H[1,1] <- 0
c <- numeric(dim(X)[2])

# 二次計画
result <- ipop(c, H, A, b, l, u, r, maxiter = 50)
w <- primal(result)

# データのプロットと超平面
plot(iris[1:100, c(4, 3)], col=ifelse(iris$Species=="setosa", "red", "blue"))
abline(-w[1]/w[3], -w[2]/w[3])
