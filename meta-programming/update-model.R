update_call <- function(object, formula., ...) {
  call <- object$call

  if (!missing(formula.)) {
    call$formula <- update.formula(formula(object), formula.)
  }

  pryr::modify_call(call, pryr::dots(...))
}

update_model <- function(object, formula., ...) {
  call <- update_call(object, formula., ...)
  eval(call, parent.frame())
}


model <- lm(mpg ~ wt, data = mtcars)
model
# Call:
# lm(formula = mpg ~ wt, data = mtcars)
#
# Coefficients:
# (Intercept)           wt
#      37.285       -5.344

model2 <- stats::update(model, formula = . ~ . + cyl)
model2
# Call:
# lm(formula = mpg ~ wt + cyl, data = mtcars)
#
# Coefficients:
# (Intercept)           wt          cyl
#      39.686       -3.191       -1.508

model3 <- update_model(model, formula = . ~ . + cyl)
model3
# Call:
# lm(formula = mpg ~ wt + cyl, data = mtcars)
#
# Coefficients:
# (Intercept)           wt          cyl
#      39.686       -3.191       -1.508

identical(model2, model3)
# [1] TRUE
