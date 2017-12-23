library(arules)

data(Income)

# Relative frequency
itemFrequencyPlot(Income)

# Extract association rules
rules <- apriori(Income)
inspect(head(sort(rules, by='support'), n=20))

# lhs                                rhs                               support confidence      lift
# 1  {}                              => {language in home=english}      0.9128854  0.9128854 1.0000000
# 2  {ethnic classification=white}   => {language in home=english}      0.6595404  0.9847991 1.0787763
# 3  {number in household=1}         => {language in home=english}      0.6495055  0.9388270 1.0284171
# 4  {education=no college graduate} => {language in home=english}      0.6343805  0.8995669 0.9854106
# 5  {years in bay area=10+}         => {language in home=english}      0.6013671  0.9300495 1.0188020
# 6  {number of children=0}          => {language in home=english}      0.5801338  0.9328812 1.0219040
# 7  {income=$0-$40,000}             => {language in home=english}      0.5578825  0.8962617 0.9817899
# 8  {number of children=0}          => {number in household=1}         0.5532286  0.8896165 1.2858951
# 9  {type of home=house}            => {language in home=english}      0.5446481  0.9129693 1.0000919
# 10 {dual incomes=not married}      => {language in home=english}      0.5426120  0.9069033 0.9934470
# 11 {age=14-34}                     => {language in home=english}      0.5248691  0.8966460 0.9822109
# 12 {number in household=1,
# number of children=0}          => {language in home=english}      0.5213787  0.9424290 1.0323629
# 13 {number of children=0,
# language in home=english}      => {number in household=1}         0.5213787  0.8987215 1.2990559
# 14 {number in household=1,
# language in home=english}      => {number of children=0}          0.5213787  0.8027318 1.2908287
# 15 {sex=female}                    => {language in home=english}      0.5122164  0.9246521 1.0128896
# 16 {income=$0-$40,000}             => {education=no college graduate} 0.5018906  0.8063084 1.1433649
# 17 {number in household=1,
# ethnic classification=white}   => {language in home=english}      0.4941827  0.9880779 1.0823680
# 18 {number of children=0,
# ethnic classification=white}   => {language in home=english}      0.4474985  0.9868505 1.0810235
# 19 {income=$0-$40,000,
# education=no college graduate} => {language in home=english}      0.4454625  0.8875688 0.9722675
# 20 {education=no college graduate,
# ethnic classification=white}   => {language in home=english}      0.4384817  0.9827249 1.0765041

summary(rules)

# summary of quality measures:
#     support         confidence          lift
#  Min.   :0.1001   Min.   :0.8000   Min.   :0.8971
#  1st Qu.:0.1101   1st Qu.:0.8436   1st Qu.:1.0813
#  Median :0.1241   Median :0.9027   Median :1.3297
#  Mean   :0.1393   Mean   :0.9021   Mean   :1.4099
#  3rd Qu.:0.1510   3rd Qu.:0.9574   3rd Qu.:1.5309
#  Max.   :0.9129   Max.   :1.0000   Max.   :4.3554
#
# mining info:
#    data ntransactions support confidence
#  Income          6876     0.1        0.8

# Equivalence CLAss Transformation
items <- eclat(Income)
inspect(head(sort(items, by='support'), n=20))

#    items                             support
# 1  {language in home=english}      0.9128854
# 2  {education=no college graduate} 0.7052065
# 3  {number in household=1}         0.6918266
# 4  {ethnic classification=white}   0.6697208
# 5  {ethnic classification=white,
#     language in home=english}      0.6595404
# 6  {number in household=1,
#     language in home=english}      0.6495055
# 7  {years in bay area=10+}         0.6465969
# 8  {education=no college graduate,
#     language in home=english}      0.6343805
# 9  {income=$0-$40,000}             0.6224549
# 10 {number of children=0}          0.6218732
# 11 {years in bay area=10+,
#     language in home=english}      0.6013671
# 12 {dual incomes=not married}      0.5983130
# 13 {type of home=house}            0.5965678
# 14 {age=14-34}                     0.5853694
# 15 {number of children=0,
#     language in home=english}      0.5801338
# 16 {income=$0-$40,000,
#     language in home=english}      0.5578825
# 17 {sex=female}                    0.5539558
# 18 {number in household=1,
#     number of children=0}          0.5532286
# 19 {type of home=house,
#     language in home=english}      0.5446481
# 20 {dual incomes=not married,
#     language in home=english}      0.5426120

summary(items)

# most frequent items:
#      language in home=english         number in household=1   ethnic classification=white
#                          2535                          1613                          1568
# education=no college graduate      dual incomes=not married                       (Other)
#                          1565                          1544                         14622
