library(dplyr)
library(ggplot2)
library(FNN)

set.seed(12345)

n <- 1000
df <- data.frame(
  X1 = rcauchy(n, location = 0, scale = 1),
  X2 = rcauchy(n, location = 0, scale = 1),
  X3 = rcauchy(n, location = 0, scale = 1),
  X4 = rcauchy(n, location = 0, scale = 1)
)

df <- df %>%
  mutate(y = if_else((X1 * X2 > 80) | (X3 * X4 > 80), T, F))

write.csv(df, "./data/synthetic-data.csv", row.names =  F)

train_X <-df[1:700,-5]
train_y <- df$y[1:700]
test_X <- df[701:n,-5]
test_y <- df$y[701:n]

table(df$y)
# FALSE  TRUE
# 980    20

# PCA
pc <- prcomp(df[,-5], scale = TRUE)
df_pc <- data.frame(pc1 = pc$x[,1], pc2 = pc$x[,2], label = df[,5])
g <- ggplot(df_pc, aes(x = pc1, y = pc2)) +
  geom_point(aes(colour = label), alpha = 1, size = 2.5) +
  labs(title = "PCA", x = "pc1", y = "pc2") +
  lims(x = c(-15, 15), y = c(-20, 20))
plot(g)

# calculate anomaly score
calc_anomaly <- function(y, idx) {
  true <- sum(y)
  false <- sum(!y)

  eps <- 0.0001
  scores <- NULL

  for (i in 1:nrow(idx)) {
    nn <- c()
    for (j in 1:ncol(idx)) {
      nn <- c(nn, y[idx[i,j]])
    }
    t_cnt <- sum(nn)
    f_cnt <- ncol(idx) - t_cnt
    score <- log((false * t_cnt + eps)/ (true * f_cnt + eps))
    scores <- c(scores, score)
  }
  scores
}

# calculate F-value
calc_f <- function(pred, y) {
  rec <- sum(y & pred) / sum(y)
  prec <- sum(y & pred) / sum(pred)
  2 * prec * rec / (prec + rec)
}

ks <- seq(1, 11, 2)
thresholds <- seq(-3, 3, 0.2)

params <- NULL

for (k in ks) {
  for (th in thresholds) {
    # leave-one-out cross validation
    res_cv <- knn.cv(train_X, cl = as.factor(train_y), k = k,
                     prob = TRUE, algorithm = "kd_tree")

    # index of k nearest neighbors
    nn_idx <- attr(res_cv, "nn.index")

    # calculate anomaly score
    scores <- calc_anomaly(train_y, nn_idx)
    pred <- if_else(scores > th, T, F)
    f_val <- calc_f(train_y, pred)

    params <- rbind(params, c(f_val, k, th))
  }
}

colnames(params) <- c("f_val", "k", "th")
best_param <- tibble::as_tibble(params) %>%
  filter(f_val == max(f_val, na.rm = T))

best_k <- first(best_param["k"][[1]])
best_th <- first(best_param["th"][[1]])

# kNN using best parameter
res <- knn.cv(train = df[,-5],
               cl = as.factor(df[,5]),
               k = best_k,
               prob = TRUE,
               algorithm = "kd_tree")

# index of k nearest neighbors
nn_idx <- attr(res, "nn.index")

scores <- calc_anomaly(df[,5], nn_idx)
pred <- if_else(scores > best_th, T, F)[701:n]

table(pred)
# pred
# FALSE  TRUE
#   294     6

table(test_y)
# test_y
# FALSE  TRUE
#   294     6

# accuracy
sum(pred == test_y)/length(test_y)
# [1] 0.98

calc_f(pred, test_y)
# [1] 0.5

# evaluation
df_eval <- df[701:n,] %>%
  mutate(label = y,
         pred = pred,
         pc1 = df_pc$pc1[701:n],
         pc2 = df_pc$pc2[701:n]) %>%
  mutate(eval = if_else((y == T & pred == T) == T, "TP",
                if_else((y == F & pred == F) == T, "TN",
                if_else((y == T & pred == F) == T, "FP",
                if_else((y == F & pred == T) == T, "FN", "")))))

df_eval$eval <- as.factor(df_eval$eval)

g <- ggplot(df_eval, aes(x = pc1, y = pc2)) +
  geom_point(aes(colour = label, shape = eval), alpha = 1, size = 2.5) +
  labs(title = "PCA", x = "pc1", y = "pc2") +
  lims(x = c(-15, 15), y = c(-20, 20))
plot(g)