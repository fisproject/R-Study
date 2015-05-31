require(kernlab)

model <- ksvm(
           Species ~., data=iris,
           type="C-bsvc",         # method
           kernel="rbfdot",
           kpar=list(sigma=0.2),  # kernel param
           C=5,                   # C param
           prob.model=TRUE        # probabilities
         )

unknown <- data.frame(
             Sepal.Length=c(5.1, 5.2, 6.7, 4.5, 7.0),
             Sepal.Width=c(3.5, 3.3, 4.1, 3.1, 5.0),
             Petal.Length=c(1.1, 1.5, 2.1, 6.7, 4.1),
             Petal.Width=c(0.2, 0.1, 0.3, 1.2, 1.4)
           )

acc <- 1-cross(model)

# decision value
result <- predict(model, unknown, type="decision")

# probabilities
predict(model, unknown, type="probabilities")
