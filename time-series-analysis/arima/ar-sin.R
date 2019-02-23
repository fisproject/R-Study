set.seed(123)

x <- 10 * sin(seq(0.1,10, 0.1)) + rnorm(100, 0.1)
plot(x, type = "l", main = "Sin wave with noise")

# AR model
model <- ar(x, aic = TRUE, method = "ols")
d <- x[eval(length(x)-model$order):length(x)]

# Long term prediction
pred <- predict(model, d, n.ahead = 5000)
plot(pred$pred, type = "l", main = "Long term prediction with AR")

# equivalent to the following
for (i in seq(1, 5000)) {
  x_new <- tail(d, model$order)
  pred <- predict(model, x_new, n.ahead = 1) # one-ahead prediction
  d <- c(d, pred$pred)
}
plot(d, type = "l")