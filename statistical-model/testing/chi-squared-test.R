d <- matrix(
  c(20, 100, 150, 30),
  nrow = 2,
  dimnames = list("gender" = c("male", "female"),
                  "transportation" = c("bicycle", "walk"))
)

#          transportation
# gender   bicycle walk
# male        20  150
# female     100   30


# Chi-squared test
chisq.test(d)
# X-squared = 127.62, df = 1, p-value < 2.2e-16
