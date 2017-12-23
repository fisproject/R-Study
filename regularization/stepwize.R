library(coefplot)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- read.csv('data/acs_ny.csv', sep=',', header=TRUE, stringsAsFactors=FALSE)
d$Income <- with(d, FamilyIncome >= 150000)
acs <- d[,c(-1,-2,-3,-13)]

null.model <- glm(
  cbind(Income, 2 - Income) ~ 1,
  data=acs,
  family=binomial(link=logit)
)

full.model <- glm(
  cbind(Income, 2 - Income) ~ NumBedrooms + NumChildren + NumPeople +
      NumRooms + NumUnits + NumVehicles + NumWorkers + OwnRent + YearBuilt +
      ElectricBill + FoodStamp + HeatingFuel + Insurance + Language,
  data=acs,
  family=binomial(link=logit)
)

model.step <- step(null.model, scope=list(lower=null.model, upper=full.model), direction='both')
summary(model.step)
# Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)
# (Intercept)             -9.060e+00  1.208e+00  -7.499 6.44e-14 ***
# Insurance                3.122e-04  1.388e-05  22.490  < 2e-16 ***
# NumWorkers               5.074e-01  2.626e-02  19.319  < 2e-16 ***
# NumRooms                 7.999e-02  7.362e-03  10.866  < 2e-16 ***
# ElectricBill             1.712e-03  1.498e-04  11.424  < 2e-16 ***
# FoodStampYes            -8.526e-01  1.146e-01  -7.442 9.94e-14 ***
# HeatingFuelElectricity   9.239e-01  3.523e-01   2.623 0.008726 **
# HeatingFuelGas           1.171e+00  3.422e-01   3.423 0.000619 ***
# HeatingFuelNone          1.285e-01  8.548e-01   0.150 0.880498
# HeatingFuelOil           1.227e+00  3.428e-01   3.579 0.000346 ***
# HeatingFuelOther         8.815e-01  4.047e-01   2.178 0.029384 *
# HeatingFuelSolar         9.772e-01  1.160e+00   0.842 0.399555
# HeatingFuelWood          1.015e-01  3.607e-01   0.281 0.778328
# NumUnitsSingle attached  2.894e+00  4.525e-01   6.396 1.59e-10 ***
# NumUnitsSingle detached  2.728e+00  4.493e-01   6.073 1.26e-09 ***
# YearBuilt1940-1949       8.378e-01  1.067e+00   0.785 0.432374
# YearBuilt1950-1959       1.007e+00  1.066e+00   0.945 0.344812
# YearBuilt1960-1969       9.501e-01  1.066e+00   0.891 0.372894
# YearBuilt1970-1979       8.637e-01  1.067e+00   0.810 0.418070
# YearBuilt1980-1989       1.115e+00  1.067e+00   1.046 0.295768
# YearBuilt1990-1999       1.089e+00  1.067e+00   1.021 0.307233
# YearBuilt2000-2004       1.123e+00  1.068e+00   1.052 0.292682
# YearBuilt2005            1.206e+00  1.076e+00   1.121 0.262394
# YearBuilt2006            1.213e+00  1.078e+00   1.124 0.260808
# YearBuilt2007            1.325e+00  1.080e+00   1.227 0.219869
# YearBuilt2008            8.853e-01  1.096e+00   0.808 0.419188
# YearBuilt2009            1.223e+00  1.098e+00   1.114 0.265089
# YearBuilt2010            1.355e+00  1.109e+00   1.221 0.222049
# YearBuiltBefore 1939     7.805e-01  1.066e+00   0.732 0.463939
# OwnRentOutright          5.634e-01  1.987e-01   2.836 0.004575 **
# OwnRentRented           -5.955e-01  9.893e-02  -6.020 1.75e-09 ***
# NumVehicles              1.477e-01  1.958e-02   7.542 4.62e-14 ***
# LanguageEnglish         -4.980e-01  7.865e-02  -6.333 2.41e-10 ***
# LanguageOther           -2.816e-01  1.507e-01  -1.869 0.061681 .
# LanguageOther European  -3.480e-01  8.992e-02  -3.870 0.000109 ***
# LanguageSpanish         -5.420e-01  9.704e-02  -5.586 2.33e-08 ***
# NumBedrooms              7.114e-02  1.762e-02   4.038 5.40e-05 ***
# NumPeople               -1.069e-01  1.982e-02  -5.392 6.96e-08 ***
# NumChildren              1.142e-01  2.160e-02   5.289 1.23e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 16989  on 22744  degrees of freedom
# Residual deviance: 13554  on 22706  degrees of freedom
# AIC: 19960

exp(model.step$coefficients)

coefplot.default(model.step, title='Coefficient Plot (Stepwise)')
