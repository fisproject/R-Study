# Kruskal–Wallis-Test
library(dplyr)

# iris
kruskal.test(Sepal.Length ~ Petal.Width, data=iris)
# Kruskal-Wallis rank sum test
#
# data:  Sepal.Length by Petal.Width
# Kruskal-Wallis chi-squared = 112.31, df = 21, p-value = 1.798e-14

# iris - setosa
se <- iris %>% filter(Species == 'setosa')
kruskal.test(Sepal.Length ~  Petal.Width, data=se)

# iris - versicolor
ve <- iris %>% filter(Species == 'versicolor')
kruskal.test(Sepal.Length ~ Petal.Width, data=ve)

# iris - virginica
vi <- iris %>% filter(Species == 'virginica')
kruskal.test(Sepal.Length ~ Petal.Width, data=vi)


# airquality
kruskal.test(Ozone ~ Month, data=airquality)
# 	Kruskal-Wallis rank sum test

# data:  Ozone by Month
# Kruskal-Wallis chi-squared = 29.2666, df = 4, p-value = 6.901e-06


# Virtual Data
x=c(2.9, 3.0, 4.5, 3.6, 4.2, 3.5, 3.5, 4.4)
y=c(3.8, 3.7, 4.0, 2.4)
z=c(2.8, 3.4, 3.5, 4.2, 3.0, 3.1)

kruskal.test(list(x, y, z))
# 	Kruskal-Wallis rank sum test

# data:  list(x, y, z)
# Kruskal-Wallis chi-squared = 1.5638, df = 2, p-value = 0.4575
