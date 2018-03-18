library(dplyr)
library(ggplot2)
library(FNN)
library(DMwR)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

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

test_X <- df[701:n,-5]
test_y <- df$y[701:n]

# SMOTE (Synthetic Minority Over-sampling Technique)
df$y <- as.factor(df$y)
df_smote <- SMOTE(y ~ ., data = df[1:700,], perc.over = 1000, k = 3, perc.under = 1000)
df_smote <- df_smote[sample(nrow(df_smote)),]
df_smote$y <- as.logical(df_smote$y)

train_X <-df_smote[,-5]
train_y <- df_smote$y

table(train_y)
# train_y
# FALSE  TRUE
#  1400   154

# PCA
pc <- prcomp(rbind(train_X, test_X), scale = TRUE)
df_pc <- data.frame(pc1 = pc$x[,1], pc2 = pc$x[,2], label = c(train_y, test_y))
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
calc_f1 <- function(pred, y) {
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
    f_val <- calc_f1(train_y, pred)

    params <- rbind(params, c(f_val, k, th))
  }
}

colnames(params) <- c("f_val", "k", "th")
best_param <- tibble::as_tibble(params) %>%
  filter(f_val == max(f_val, na.rm = T))

best_k <- first(best_param["k"][[1]])
best_th <- first(best_param["th"][[1]])

# kNN using best parameter
res <- knn.cv(train = rbind(train_X, test_X),
               cl = as.factor(c(train_y, test_y)),
               k = best_k,
               prob = TRUE,
               algorithm = "kd_tree")

# index of k nearest neighbors
nn_idx <- attr(res, "nn.index")

# evaluation
scores <- calc_anomaly(c(train_y, test_y), nn_idx)
pred <- if_else(scores > best_th, T, F)[(length(train_y)+1):(length(train_y)+length(test_y))]

table(pred)
# pred
# FALSE  TRUE
#   288    12

table(test_y)
# test_y
# FALSE  TRUE
#   294     6

# accuracy
sum(pred == test_y)/length(test_y)
# [1] 0.96

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

df_eval %>%
  group_by(eval) %>%
  summarise(n = n())
# # A tibble: 4 x 2
#   eval      n
#   <fct> <int>
# 1 FN        9
# 2 FP        3
# 3 TN      285
# 4 TP        3

calc_f1(pred, test_y)
# [1] 0.3333333

g <- ggplot(df_eval, aes(x = pc1, y = pc2)) +
  geom_point(aes(colour = label, shape = eval), alpha = 1, size = 2.5) +
  labs(title = "PCA", x = "pc1", y = "pc2") +
  lims(x = c(-15, 15), y = c(-20, 20))
plot(g)
