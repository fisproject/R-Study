require(parallel)
require(doParallel)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

set.seed(5127151)

acs <- read.csv('data/acs_ny.csv', sep=',', header=TRUE, stringsAsFactors=FALSE)
acs$Income <- with(acs, FamilyIncome >= 150000)

acsX <- build.x(
  Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits +
  NumVehicles + NumWorkers + OwnRent + YearBuilt + HouseCosts +
  ElectricBill + FoodStamp + HeatingFuel + Insurance + Language - 1,
  data=acs, contrasts=FALSE
)

acsY <- build.y(
  Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits +
  NumVehicles + NumWorkers + OwnRent + YearBuilt + HouseCosts +
  ElectricBill + FoodStamp + HeatingFuel + Insurance + Language - 1,
  data=acs
)

# range of search
theFolds <- sample(rep(x=1:5, length.out=nrow(acsX)))
alphas <- seq(from=0.5, to=1, by=0.05)
cluster <- makeCluster(2)

registerDoParallel(cluster)

before <- Sys.time()

acsDouble <- foreach(i=1:length(alphas),
 .errorhandling='remove',
 .inorder=FALSE,
 .multicombine=TRUE,
 .export=c('acsX', 'acsY', 'alphas', 'theFolds'),
 .packages='glmnet') %dopar% {
   print(alphas[i])
   acs.cv1 <- cv.glmnet(x=acsX, y=acsY, family='binomial', nfold=5, foldid=theFolds, alpha=alphas[i])
 }

after <- Sys.time()

after - before
# Time difference of 1.563828 mins

stopCluster(cluster)

sapply(acsDouble, class)

extractGlmnetInfo <- function(obj) {
    lambda.min <- obj$lambda.min
    lambda.1se <- obj$lambda.1se
    which.min <- which(obj$lambda == lambda.min)
    which.1se <- which(obj$lambda == lambda.1se)

    data.frame(
      lambda.min=lambda.min,
      error.min=obj$cvm[which.min],
      lambda.1se=lambda.1se,
      error.1se=obj$cvm[which.1se]
      )
}

# lapply for list
alphaInfo <- Reduce(rbind, lapply(acsDouble, extractGlmnetInfo))

alphaInfo$Alpha <- alphas
# alphaInfo
# lambda.min error.min  lambda.1se error.1se Alpha
# 1  0.0008165096 0.7749260 0.009172048 0.7801316  0.50
# 2  0.0008146529 0.7749227 0.008338226 0.7799511  0.55
# 3  0.0007467652 0.7749201 0.007643373 0.7798048  0.60
# 4  0.0006893217 0.7749181 0.007055422 0.7796840  0.65
# 5  0.0006400845 0.7749165 0.006551463 0.7795832  0.70
# 6  0.0006556591 0.7749148 0.006710874 0.7802125  0.75
# 7  0.0006146804 0.7749133 0.006291445 0.7801291  0.80
# 8  0.0005785227 0.7749121 0.005921360 0.7800569  0.85
# 9  0.0005463826 0.7749110 0.005592395 0.7799939  0.90
# 10 0.0005176256 0.7749102 0.005298059 0.7799386  0.95
# 11 0.0004917443 0.7749094 0.005033156 0.7798906  1.00
