require(ggplot2)
require(ggfortify)
require(purrr)

d <- iris

p1 <- ggplot(d, aes(Sepal.Width, Sepal.Length, color=Species)) + labs(title="Sepal")
p2 <- ggplot(d, aes(Petal.Width, Petal.Length, color=Species)) + labs(title="Petal")
p3 <- ggplot(d, aes(Sepal.Length, Petal.Length, color=Species)) + labs(title="Length")
p4 <- ggplot(d, aes(Sepal.Width, Petal.Width, color=Species)) + labs(title="Width")

p <- new('ggmultiplot', plots=list(p1, p2, p3, p4), ncol=2)
p[1:4] <- p[1:4] + geom_point() + stat_smooth(method = 'lm')
p

# cluster number 1 to 9
kclusts <- purrr::map(1:9, ~ kmeans(d[-5], .))
autoplot(kclusts, data=d[-5], ncol=3) + theme(legend.position="none")
