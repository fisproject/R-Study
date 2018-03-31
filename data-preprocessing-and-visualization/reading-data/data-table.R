library(data.table)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# csv
baseball <- fread("data/baseball.csv", header = TRUE)

# ltsv
access_log <- fread("data/access.log", header = FALSE)

names(access_log) <- c("domain", "host", "user", "time", "method", "path", "protocol",
    "status", "size", "referer", "agent", "response_time", "cookie", "set_cookie",
    "upstream_addr", "upstream_cache_status", "upstream_response_time")
