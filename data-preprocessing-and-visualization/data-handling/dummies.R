library(dummies)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- read.csv("data/baseball.csv")

d$team_rank <- as.ordered(d$team_rank)
d$birthday <- as.Date(d$birthday)
d$last_game <- as.Date(d$last_game)
# 'data.frame':	8 obs. of  5 variables:
#  $ name     : Factor w/ 8 levels "荻野　貴司","角中　勝也",..: 4 7 2 3 5 6 1 8
#  $ team     : Factor w/ 3 levels "広島東洋カープ",..: 1 1 2 1 1 2 2 3
#  $ birthday : Date, format: "1990-03-11" "1988-04-11" "1987-05-25" ...
#  $ last_game: Date, format: "2015-07-01" "2015-06-30" "2015-07-01" ...
#  $ team_rank: Ord.factor w/ 3 levels "1"<"3"<"4": 3 3 2 3 3 2 2 1

# column name
d2 <- dummy.data.frame(d, sep = ".", names = c("team"))

# type
d3 <- dummy.data.frame(d, sep = ".", dummy.classes = c("ordered"))
