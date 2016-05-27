require(pwr)

# Calc power
power.t.test(
  n = 10, # sample size
  delta = 0.5, # true difference in means
  sd = 1, # standard deviation
  sig.level = 0.05, # significance level
  power = NULL, # power of test
  strict = TRUE # two-side case
)
# Two-sample t test power calculation
#
#          n = 10
#      delta = 0.5
#         sd = 1
#  sig.level = 0.05
#      power = 0.1850957
# alternative = two.sided

# Calc sample size
power.t.test(
  n = NULL,
  delta = 0.2,
  sd = 1,
  sig.level = 0.05,
  type = "paired",
  power = 0.8,
  alternative = "one.sided"
)
# Paired t test power calculation
#
#          n = 155.9257
#      delta = 0.2
#         sd = 1
#  sig.level = 0.05
#      power = 0.8
# alternative = one.sided

# Using {pwr}
pwr.t.test(
    n = NULL,
    d = 0.5, # Effect size
    sig.level = 0.05,
    power = 0.8
)
