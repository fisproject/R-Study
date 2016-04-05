require(dplyr)
require(jqr)
require(jsonlite)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# from JSON file
data.json <- fromJSON("data/attractions.json")
data.df <- data.frame(data.json)

# from JSON text
data.raw <- '[{"code":1,"name":"カリブの海賊"},{"code":2,"name":"ビッグサンダー・マウンテン"}]'

jq(data.raw, ".")
# [
#     {
#         "code": 1,
#         "name": "カリブの海賊"
#     },
#     {
#         "code": 2,
#         "name": "ビッグサンダー・マウンテン"
#     }
# ]

data.index <- data.raw %>% index() # same jqr::jq()

data.index[1] %>% keys()
# [
#     "code",
#     "name"
# ]

data.index[1] %>% types()
# [
#     "number",
#     "string"
# ]

data.index[1] %>% del(name)
# {
#     "code": 1
# }

data.raw %>% index %>%  dotstr(name)
# [
#     "カリブの海賊",
#     "ビッグサンダー・マウンテン"
# ]

data.df <- data.raw %>% index() %>% string() %>% fromJSON(txt = .)
