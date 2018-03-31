d <- matrix(
  c(2, 10, 15, 3),
  nrow = 2,
  dimnames = list("gender" = c("male", "female"),
                  "transportation" = c("bicycle", "walk"))
)

#          transportation
# gender   bicycle walk
# male         2   15
# female      10    3

# Fisher exact test
fisher.test(d)
# p-value = 0.0005367
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  0.003325764 0.363182271
# sample estimates:
# odds ratio
# 0.04693661
