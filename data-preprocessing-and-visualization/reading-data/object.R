frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

baseball <- readr::read_csv("data/baseball.csv")

# Save R Objects
save(baseball, file = "data/baseball.RData")

# Reload Saved Datasets
load("data/baseball.RData")