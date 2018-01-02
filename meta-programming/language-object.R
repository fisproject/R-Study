# 1. quote
call1 <- quote(rnorm(5))

call1
# rnorm(5)

typeof(call1)
# [1] "language"

class(call1)
# [1] "call"

as.list(call1)
# [[1]]
# rnorm
#
# [[2]]
# [1] 5

str(as.list(call1))
# List of 2
#  $ : symbol rnorm
#  $ : num 5

call2 <- quote(sqrt(1 + x^2))

pryr::call_tree(call2)
# \- ()
#   \- `sqrt
#   \- ()
#     \- `+
#     \-  1
#     \- ()
#       \- `^
#       \- `x
#       \-  2

call1[[1]] <- quote(runif)

call1
# runif(5)

call1[[3]] <- -1
# names(call1)[[3]] <- "min"

call1
# runif(5, min = -1)


# 2. substitute
substitute(f(x + f(y)), list(f = quote(sin)))
# sin(x + sin(y))

call3 <- quote(rnorm(5, mean = 3))
call4 <- call("rnorm", 5, mean = 3)

identical(call3, call4)
# [1] TRUE


# 3. eval
eval(call3)
# [1] 3.148109 3.097713 1.560468 3.146791 4.420316
