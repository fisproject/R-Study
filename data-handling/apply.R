df <- data.frame(x = c(1,2,3,4,5), y = c(6,7,8,9,10), z = c(11,12,13,14,15))

normalize <- function(v) {
  v.min <- min(v)
  v.max <- max(v)
  l <- lapply(v, function(x) {return((x - v.min) / (v.max - v.min))})
  return(unlist(l))
}

# Apply to each row
row.norm <- apply(df, 1, normalize)
# > t(row.norm)
#      x   y z
# [1,] 0 0.5 1
# [2,] 0 0.5 1
# [3,] 0 0.5 1
# [4,] 0 0.5 1
# [5,] 0 0.5 1

# Apply to each column
col.norm <- apply(df, 2, normalize)
# > col.norm
#         x    y    z
# [1,] 0.00 0.00 0.00
# [2,] 0.25 0.25 0.25
# [3,] 0.50 0.50 0.50
# [4,] 0.75 0.75 0.75
# [5,] 1.00 1.00 1.00
