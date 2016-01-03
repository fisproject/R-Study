d <- matrix(
  c(20, 100, 150, 30),
  nrow=2,
  dimnames=list(c("male", "female"), c("bicycle", "walk"))
)

# Chi-squared test
chisq.test(d)
# X-squared = 127.62, df = 1, p-value < 2.2e-16
