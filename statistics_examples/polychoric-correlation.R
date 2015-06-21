require(polycor)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

data <- read.csv("data/satisfaction.csv")
# id satisfaction price gender
# 1   1            5     4       1
# 2   2            2     1       0
# 3   3            3     2       0
# 4   4            1     2       0
# 5   5            1     0       1
# 6   6            2     3       0
# 7   7            3     1       1
# 8   8            3     3       0
# 9   9            4     5       1
# 10 10            0     1       0

# pearson
cor(data)
#               id           satisfaction  price   gender
# id            1.00000000   -0.2925090 0.07106691 0.0000000
# satisfaction -0.29250897    1.0000000 0.72410514 0.4859127
# price         0.07106691    0.7241051 1.00000000 0.1666667
# gender        0.00000000    0.4859127 0.16666667 1.0000000


# polychoric
polychor(data$satisfaction, data$price)
# [1] 0.7377534
polychor(data$satisfaction, data$gender)
# [1] 0.6300075
polychor(data$price, data$gender)
# [1] 0.08929742
