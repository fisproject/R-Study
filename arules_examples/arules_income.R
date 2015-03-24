library(arules)

data(Income)

# Relative frequency
itemFrequencyPlot(Income)

# Extract association rules
rules <- apriori(Income)
inspect(head(sort(rules, by='support'), n=20))
summary(rules)

# summary of quality measures:
#     support         confidence          lift
#  Min.   :0.1001   Min.   :0.8000   Min.   :0.8971
#  1st Qu.:0.1101   1st Qu.:0.8436   1st Qu.:1.0813
#  Median :0.1241   Median :0.9027   Median :1.3297
#  Mean   :0.1393   Mean   :0.9021   Mean   :1.4099
#  3rd Qu.:0.1510   3rd Qu.:0.9574   3rd Qu.:1.5309
#  Max.   :0.9129   Max.   :1.0000   Max.   :4.3554


# Equivalence CLAss Transformation
items <- eclat(Income)
inspect(head(sort(items, by='support'), n=20))
summary(items)

# summary of quality measures:
#     support
#  Min.   :0.1001
#  1st Qu.:0.1121
#  Median :0.1297
#  Mean   :0.1505
#  3rd Qu.:0.1646
#  Max.   :0.9129
