# 2. higher-order function
f <- function(x, y) {
  op <- if (x > y) `+` else `-`
  op(x, y)
}

f(1, 2)
# [1] -1
f(2, 1)
# [1] 3

product <- function(x, y, z) {
  x * y * z
}

combine <- function(f, x, y, z) {
  f(x, y, z)
}

combine(product, 3, 4, 5)
# [1] 60

# lapply implement in R
lapply <- function(x, f, ...) {
  res <- list()
  for (i in seq_along(x)) {
    res[[i]] <- f(x[i], ...)
  }
  res
}

lapply(1:3, `+`, 3)
# [[1]]
# [1] 4
#
# [[2]]
# [1] 5
#
# [[3]]
# [1] 6

parallel::mclapply(1:3, `+`, 3)
# [[1]]
# [1] 4
#
# [[2]]
# [1] 5
#
# [[3]]
# [1] 6
