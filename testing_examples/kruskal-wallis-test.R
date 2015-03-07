# Kruskal–Wallis-Test Example

# iris
kruskal.test(Sepal.Length ~ Petal.Width, data=iris) 

# iris - setosa
kruskal.test(list(iris$Sepal.Length[1:50], iris$Sepal.Width[1:50], iris$Petal.Length[1:50], iris$Petal.Width[1:50])) 
# iris - versicolor
kruskal.test(list(iris$Sepal.Length[51:100], iris$Sepal.Width[51:100], iris$Petal.Length[51:100], iris$Petal.Width[51:100])) 
# iris - virginica
kruskal.test(list(iris$Sepal.Length[101:150], iris$Sepal.Width[101:150], iris$Petal.Length[101:150], iris$Petal.Width[101:150])) 

# airquality
kruskal.test(Ozone ~ Month, data=airquality)
# > # airquality
# > kruskal.test(Ozone ~ Month, data=airquality)

# 	Kruskal-Wallis rank sum test

# data:  Ozone by Month
# Kruskal-Wallis chi-squared = 29.2666, df = 4, p-value = 6.901e-06

# Original Data
x=c(2.9, 3.0, 4.5, 3.6, 4.2, 3.5, 3.5, 4.4)
y=c(3.8, 3.7, 4.0, 2.4)
z=c(2.8, 3.4, 3.5, 4.2, 3.0, 3.1)

kruskal.test(list(x, y, z))
# > kruskal.test(list(x,y,z))

# 	Kruskal-Wallis rank sum test

# data:  list(x, y, z)
# Kruskal-Wallis chi-squared = 1.5638, df = 2, p-value = 0.4575