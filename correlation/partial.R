require(psych)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- read.csv("data/accident.csv")
#     city post accident population
# 1     1  160       58         85
# 2     2  175       68         91
# 3     3  158       55         79
# 4     4  165       63         88
# 5     5  177       66         95
# 6     6  166       67         89
# 7     7  170       59         87
# 8     8  171       62         91
# 9     9  173       65         93
# 10   10  168       61         90

partial.r(d, c(2, 4), 3)
#   partial correlations
#            post population
# post       1.00       0.76
# population 0.76       1.00

partial.r(d, c(2, 3), 4)
#   partial correlations
#          post accident
# post     1.00     0.04
# accident 0.04     1.00
