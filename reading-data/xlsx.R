require(readxl)
require(openxlsx)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

# reading xlsx by readxl
data <- read_excel("data/disease.xlsx", sheet = 1)

# reading xlsx by openxlsx
data2 <- read.xlsx("data/disease.xlsx", sheet = 1)

# write xlsx by openxlsx
write.xlsx(iris, file = "data/iris.xlsx", colNames = TRUE)
