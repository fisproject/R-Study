d <- matrix(
  c(794, 86, 150, 570),
  nrow = 2,
  dimnames = list("first" = c("support", "non-support"),
                  "second" = c("support", "non-support"))
)

#               second
# first         support non-support
# support         794         150
# non-support      86         570

mcnemar.test(d)
# McNemar's Chi-squared test with continuity correction
#
# data:  d
# McNemar's chi-squared = 16.818, df = 1, p-value = 4.115e-05
