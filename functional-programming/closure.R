# 1. closure
add <- function(y) {
  function(x) {
    x + y
  }
}

add1 <- add(1)
add2 <- add(2)

add1(10)
# [1] 11
add2(10)
# [1] 12
environment(add1)$y
# [1] 1
environment(add2)$y
# [1] 2

color_line <- function(color) {
  function(...) {
    plot(..., type = "l", lty = 1, col = color)
  }
}

red_line <- color_line("blue")
red_line(rnorm(50), main = "Blue Line Plot")
