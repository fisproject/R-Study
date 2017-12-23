library(dplyr)
library(data.table)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

ratings <- fread('/path/to/your/MovieLens/ml-latest/ratings.csv')
movies <- fread('/path/to/your/MovieLens/ml-latest/movies.csv')

head(ratings)
#    userId movieId rating  timestamp
# 1:      1      50    4.0 1329753504
# 2:      1     296    4.0 1329753602
# 3:      1     318    4.5 1329753494
# 4:      1     527    4.5 1329753507
# 5:      1     541    3.0 1329753607
# 6:      1     608    4.0 1329753638

nrow(ratings)
# [1] 21622187

# Function chain
top10 <- ratings %>%
    group_by(movieId) %>%
    summarize(count = n(), rating.mean = mean(rating)) %>%
    filter(count > 10000) %>%
    arrange(desc(rating.mean)) %>%
    head(10)
#     movieId count rating.mean
# 1       318 70754    4.444908
# 2       858 46077    4.356642
# 3        50 49728    4.329100
# 4       527 55613    4.303104
# 5      1221 30165    4.269103
# 6      2019 12173    4.263657
# 7       904 18502    4.256405
# 8      1193 33904    4.247331
# 9       912 26729    4.242920
# 10      750 24474    4.235086

best_movie_info <- movies %>% filter(movieId == top10[1,1])
# movieId                            title      genres
# 1:     318 Shawshank Redemption, The (1994) Crime|Drama
