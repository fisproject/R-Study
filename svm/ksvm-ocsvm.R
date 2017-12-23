library(kernlab)
library(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

theme_set(theme_gray(base_family="HiraKakuProN-W3"))

set.seed(1500)

df <- data.frame(miles = airmiles[1:24], class = 1)

svm <- ksvm(
  x = class ~ .,
  data = df,
  type = "one-svc",
  kernel = "rbfdot",
  kpar = list(sigma = 0.1),
  nu = 0.1 # 10% from the origin
)

preds <- predict(svm)
pred <- ifelse(preds == TRUE, 1, 0)
res <- data.frame(df, pred)
res$idx <- seq.int(nrow(res))

g <- ggplot(res, aes(x = idx, y = miles, colour = pred)) + geom_point() +
  labs(title = "OCSVM", x = "index", y = "miles")
plot(g)
