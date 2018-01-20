# log likelihood function
# oxoxxoxxxo => 4/10
llf <- function(x) log(x^4 * (1-x)^6)

exp(llf(0.1))
# [1] 5.31441e-05
exp(llf(0.3))
# [1] 0.0009529569

# optimization
optimize(llf, c(0, 1), maximum = TRUE)
optim(c(0.1), llf, method = "Brent", lower = 0, upper = 1, control = list(fnscale = -1))

func <- deriv(~ log(x^4 * (1-x)^6), "x", func = T)
class(func)
# [1] "function"
typeof(func)
# [1] "closure"
plot(func, xlab = "x", ylab = "L(x)")