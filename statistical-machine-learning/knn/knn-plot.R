library(class)

df <- ElemStatLearn::mixture.example

tibble::glimpse(df)
# List of 8
#  $ x       : num [1:200, 1:2] 2.5261 0.367 0.7682 0.6934 -0.0198 ...
#  $ y       : num [1:200] 0 0 0 0 0 0 0 0 0 0 ...
#  $ xnew    : matrix [1:6831, 1:2] -2.6 -2.5 -2.4 -2.3 -2.2 -2.1 -2 -1.9 -1.8 -1.7 ...
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : chr [1:6831] "1" "2" "3" "4" ...
#   .. ..$ : chr [1:2] "x1" "x2"
#  $ prob    : atomic [1:6831] 3.55e-05 3.05e-05 2.63e-05 2.27e-05 1.96e-05 ...
#   ..- attr(*, ".Names")= chr [1:6831] "1" "2" "3" "4" ...
#  $ marginal: atomic [1:6831] 6.65e-15 2.31e-14 7.62e-14 2.39e-13 7.15e-13 ...
#   ..- attr(*, ".Names")= chr [1:6831] "1" "2" "3" "4" ...
#  $ px1     : num [1:69] -2.6 -2.5 -2.4 -2.3 -2.2 -2.1 -2 -1.9 -1.8 -1.7 ...
#  $ px2     : num [1:99] -2 -1.95 -1.9 -1.85 -1.8 -1.75 -1.7 -1.65 -1.6 -1.55 ...
#  $ means   : num [1:20, 1:2] -0.2534 0.2667 2.0965 -0.0613 2.7035 ...

train_X <- df$x
train_y <- df$y
test_X <- df$xnew

# kNN
mod5 <- knn(train_X, test = test_X, cl = train_y , k = 5, prob = TRUE)

prob <- attr(mod5, "prob")
prob5 <- matrix(ifelse(mod5 == "1", prob, 1 - prob),
                length(df$px1),
                length(df$px2))

# plot
col_y <- ifelse(train_y == 1, "coral", "cornflowerblue")
col_pred <- ifelse(prob5 > 0.5, "coral", "cornflowerblue")

par(mar = rep(2,4))
contour(df$px1, df$px2, prob5,
        levels = 0.5, labels = "",
        xlab = "", ylab = "", main = "5-nearest neighbour",
        axes = FALSE)
points(train_X, col = col_y)
gd <- expand.grid(x = df$px1, y = df$px2)
points(gd,pch = ".", cex = 1.2, col = col_pred)
box()
