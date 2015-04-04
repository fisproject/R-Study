require(kernlab)

n <- 150 # number of data points
p <- 2 # dimension

sigma <- 1 # variance of the distribution
meanpos <- 0 # centre of the distribution of positive examples
meanneg <- 3 # centre of the distribution of negative examples
npos <- round(n/2) # number of positive examples
nneg <- n-npos # number of negative examples

# Generate the positive and negative examples
xpos <- matrix(rnorm(npos*p,mean=meanpos, sd=sigma), npos, p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg, sd=sigma), npos ,p)
x <- rbind(xpos, xneg)
# Generate the labels
y <- matrix(c(rep(1, npos), rep(-1, nneg)))


## Prepare a training and a test set ##
ntrain <- round(n*0.8) # number of training examples
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
istest <- y[-tindex]
istrain=rep(0, n)
istrain[tindex]=1

# train the SVM
svp <- ksvm(xtrain, ytrain, type="C-svc", kernel="vanilladot", C=100, scaled=c())

# Look and understand what svp contains
# General summary
svp

# Attributes that you can access
attributes(svp)

# For example, the support vectors
alpha(svp)
alphaindex(svp)
b(svp)

# Use the built-in function to pretty-plot the classifier
plot(svp,data=xtrain)
