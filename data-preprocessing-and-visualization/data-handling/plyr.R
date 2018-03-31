library(plyr)

# SQL: JOIN
first <- ddply(baseball, "id", summarise, first = min(year))
baseball.joined <- baseball %>% join(first, by = "id")
