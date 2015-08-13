require(arules)

data(Groceries)

# Relative frequency
itemFrequencyPlot(Groceries)

# Extract association rules
rules <- apriori(Groceries, p=list(support=0.01, confidence=0.5, maxlen=4, ext=TRUE))
inspect(head(sort(rules, by='lift'), n=20))
summary(rules)

# summary of quality measures:
#     support          confidence      lhs.support           lift
#  Min.   :0.01007   Min.   :0.5000   Min.   :0.01729   Min.   :1.984
#  1st Qu.:0.01174   1st Qu.:0.5151   1st Qu.:0.02089   1st Qu.:2.036
#  Median :0.01230   Median :0.5245   Median :0.02430   Median :2.203
#  Mean   :0.01316   Mean   :0.5411   Mean   :0.02454   Mean   :2.299
#  3rd Qu.:0.01403   3rd Qu.:0.5718   3rd Qu.:0.02598   3rd Qu.:2.432
#  Max.   :0.02227   Max.   :0.5862   Max.   :0.04342   Max.   :3.030
#
# mining info:
#       data ntransactions support confidence
#  Groceries          9835    0.01        0.5
