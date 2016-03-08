require(glmnet)
require(useful)
require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# coefplot for glmnet model
coefplot <- function(model, title) {
  theCoef <- as.matrix(coef(model, s = 'lambda.1se'))
  coefDF <- data.frame(Value = theCoef, Coefficient = rownames(theCoef))
  coefDF <- coefDF[nonzeroCoef(coef(model, s = 'lambda.1se')),]
  g <- ggplot(
    coefDF,
    aes(
      x = X1,
      y = reorder(Coefficient, X1)
    ))
  g + geom_vline(xintercept = 0, color = 'grey', linetype = 2) + # or lineplot ?
    geom_point(color='blue') + labs(x = 'Value', y = 'Coefficient', title = title)
}

set.seed(5127151)

acs <- read.csv('data/acs_ny.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)
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

acs.glmnet <- glmnet(x = acsX, y = acsY, family = 'binomial', alpha = 1)

# Cross Validation - Lasso
acs.cv1 <- cv.glmnet(x = acsX, y = acsY, family = 'binomial', nfold = 5, alpha = 1)
acs.cv1$lambda.min
acs.cv1$lambda.1se # one standard deviation
plot(acs.cv1, main='Lasso alpha=1.0')

acs.cv1.vi <- acs.glmnet$beta[, match(acs.cv1$lambda.min, acs.cv1$lambda)] # variable importance
dotchart(tail(sort(acs.cv1.vi), 30), main="variable importance")

coef(acs.cv1, s='lambda.1se') # var selection

acs.cv1.pred <- predict(acs.cv1, s = "lambda.1se", newx = acsX)

coefplot(acs.cv1, 'Coefficient Plot (alpha=1.0)')

plot(acs.cv1$glmnet.fit, xvar = 'lambda', main = 'solution path (alpha=1.0)', label = TRUE) # solution path
abline(v = log(c(acs.cv1$lambda.min, acs.cv1$lambda.1se)), lty = 2)


# Cross Validation - Ridge
acs.cv2 <- cv.glmnet(x = acsX, y = acsY, family = 'binomial', nfold = 5, alpha = 0)
acs.cv2$lambda.min
acs.cv2$lambda.1se # one standard deviation

plot(acs.cv2, main = 'Ridge (alpha=0.0)')

coef(acs.cv2, s = 'lambda.1se')

acs.cv2.pred <- predict(acs.cv2, s = "lambda.1se", newx = acsX)

coefplot(acs.cv2, 'Coefficient Plot (alpha=0.0)')

plot(acs.cv2$glmnet.fit, xvar = 'lambda', main = 'solution path (alpha=0.0)', label = TRUE) # solution path
abline(v = log(c(acs.cv2$lambda.min, acs.cv2$lambda.1se)), lty = 2)


# Cross Validation - min 1se (alpha=0.70)
acs.cv3 <- cv.glmnet(x = acsX, y = acsY, family = 'binomial', nfold = 5, alpha = 0.70)
acs.cv3$lambda.min
acs.cv3$lambda.1se # one standard deviation

plot(acs.cv3, main='alpha=0.70')

coef(acs.cv3, s='lambda.1se')

acs.cv3.pred <- predict(acs.cv3, s = "lambda.1se", newx = acsX)

coefplot(acs.cv3, 'Coefficient Plot (alpha=0.70)')

acs.cv3.vi <- acs.glmnet$beta[, match(acs.cv3$lambda.min, acs.cv3$lambda)] # variable importance
dotchart(tail(sort(acs.cv3.vi), 30), main = "variable importance")

plot(acs.cv3$glmnet.fit, xvar = 'lambda', main = 'solution path (alpha=0.7)', label = TRUE) # solution path
abline(v = log(c(acs.cv3$lambda.min, acs.cv3$lambda.1se)), lty = 2)
