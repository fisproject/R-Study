library(aws.s3)
library(dplyr)

Sys.setenv("AWS_ACCESS_KEY_ID" = "[ACCESS_KEY]",
           "AWS_SECRET_ACCESS_KEY" = "[SECRET_KEY]")

bucket_name <- "t2sy.example"
# # The objects in the bucket are below.
# {"name": "Alice", "age": 30, "gender": "F"}
# {"name": "Bob", "age": 25,"gender": "M"}
# {"name": "Eve", "age": 20, "gender": "F"}

objects.json <- get_bucket(bucket_name) %>%
  lapply(function(x) {
    get_object(x$Key, bucket = bucket_name) %>%
      rawToChar() %>%
      jsonlite::fromJSON()})

df <- do.call(function(...) rbind(data.frame(), ...), objects.json)
rownames(df) <- NULL
