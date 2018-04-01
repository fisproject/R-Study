library(pryr)

# delayed binding (=delayedAssign)
system.time(b %<d-% {Sys.sleep(1); 1})
# ユーザ   システム       経過
# 0.001      0.000      0.000

promise_info(b)
# $code
# {
#     Sys.sleep(1)
#     1
# }
#
# $env
# <environment: R_GlobalEnv>
#
# $evaled
# [1] FALSE
#
# $value
# NULL

system.time(b)
# ユーザ   システム       経過
#   0.003      0.003      1.001

promise_info(b)
# $code
# {
#     Sys.sleep(1)
#     1
# }
#
# $env
# NULL
#
# $evaled
# [1] TRUE
#
# $value
# [1] 1

# active binding (=makeActiveBinding)
x %<a-% runif(1)
x
# [1] 0.2884783

x
# [1] 0.05777214

# unevaluated dots
y <- 2

f <- function(...) {
  dots(...)
}
f(a = 1, y, b =)
# $b

dots <- function(a, y, ...) {
  eval(substitute(alist(...)))
}
dots(a = 1, y, b =)
# $b

f <- function(...) {
  named_dots(...)
}
f(a = 1, y, b =)
# $a
# [1] 1
#
# $y
# y
#
# $b
