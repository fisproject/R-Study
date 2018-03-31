library(stringr)

# Combining strings
str_c("prefix-", c("a", "b", "c"), "-suffix")
#[1] "prefix-a-suffix" "prefix-b-suffix" "prefix-c-suffix"

str_c("prefix-", c("a", "b", "c", NA), "-suffix")
# [1] "prefix-a-suffix" "prefix-b-suffix" "prefix-c-suffix" NA

str_c("prefix-", if (FALSE) c("a", "b", "c", NA), "-suffix")
# [1] "prefix--suffix"

# Subsetting strings
str_sub(c("Apple", "Banana", "Pear"), 1, 3)
# [1] "App" "Ban" "Pea"

str_sub(c("Apple", "Banana", "Pear"), -3, -1)
# [1] "ple" "ana" "ear"

# Sorting
str_sort(c("Apple", "Banana", "Pear"), locale = "en")
# [1] "Apple"  "Banana" "Pear"

str_sort(c("Apple", "Banana", "eggplant"), locale = "en")
# [1] "Apple"    "Banana"   "eggplant"

str_sort(c("Apple", "Banana", "eggplant"), locale = "haw")
# [1] "Apple"    "eggplant" "Banana"

# Match
str_match(c("grey", "gray"), "gr(e|a)y")
#      [,1]   [,2]
# [1,] "grey" "e"
# [2,] "gray" "a"

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')
str_match(x, "C{2}")
#      [,1]
# [1,] "CC"

str_match(x, "CC?")
#      [,1]
# [1,] "CC"

str_match(x, "CC+")
#      [,1]
# [1,] "CCC"

str_match(x, 'C[LX]+')
#      [,1]
# [1,] "CLXXX"

str_match(x, "C{2}")
#      [,1]
# [1,] "CC"

str_match(x, "C{2,}")
#      [,1]
# [1,] "CCC"

str_match(x, "C{2,3}")
#      [,1]
# [1,] "CCC"

x <- "This is a sentence."
str_view_all(x, boundary("word"))
str_extract_all(x, boundary("word"))
# [[1]]
# [1] "This"     "is"       "a"        "sentence"

# Detect matches
x <- c("apple", "banana", "pear")
str_detect(x, "e")
# [1]  TRUE FALSE  TRUE

dplyr::glimpse(words)
# chr [1:980] "a" "able" "about" "absolute" "accept" "account" "achieve" ...

sum(str_detect(words, "^t"))
# [1] 65

mean(str_detect(words, "[aeiou]$"))
# [1] 0.2765306

str_subset(words, "x$")
# [1] "box" "sex" "six" "tax"

x <- c("apple", "banana", "pear")
str_detect(x, "e")
# [1]  TRUE FALSE  TRUE

sum(str_detect(words, "^t"))
# [1] 65

mean(str_detect(words, "[aeiou]$"))
# [1] 0.2765306

# Extract matches
x <- c("apple", "banana", "pear")
str_extract(x, "an")
# [1] NA   "an" NA

str_extract(x, ".a.")
# [1] NA    "ban" "ear"

str_extract(x, "^a")
# [1] "a" NA  NA

str_extract(x, "a$")
# [1] NA  "a" NA

x <- "a\\b"
str_extract(x, "\\\\")
# [1] "\\"

# Count
x <- c("apple", "banana", "pear")
str_count(x, "a")
# [1] 1 3 1

mean(str_count(words, "[aeiou]"))
# [1] 1.991837

str_count("abababa", "aba")
# [1] 2

# Extract matches
dplyr::glimpse(sentences)
# chr [1:720] "The birch canoe slid on the smooth planks." ...

head(sentences)
# [1] "The birch canoe slid on the smooth planks."
# [2] "Glue the sheet to the dark blue background."
# [3] "It's easy to tell the depth of a well."
# [4] "These days a chicken leg is a rare dish."
# [5] "Rice is often served in round bowls."
# [6] "The juice of lemons makes fine punch."

colors <- c("red", "orange", "yellow", "green", "blue", "purple")
color_match <- str_c(colors, collapse = "|")
color_match
# [1] "red|orange|yellow|green|blue|purple"

has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match)
head(matches)
# [1] "blue" "blue" "red"  "red"  "red"  "blue"

# Grouped matches
noun <- "(a|the) ([^ ]+)"
has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)

has_noun %>%
  str_extract(noun)
# [1] "the smooth" "the sheet"  "the depth"  "a chicken"  "the parked"
# [6] "the sun"    "the huge"   "the ball"   "the woman"  "a helps"

has_noun %>%
  str_match(noun)
# [,1]         [,2]  [,3]
# [1,] "the smooth" "the" "smooth"
# [2,] "the sheet"  "the" "sheet"
# [3,] "the depth"  "the" "depth"
# [4,] "a chicken"  "a"   "chicken"
# [5,] "the parked" "the" "parked"
# [6,] "the sun"    "the" "sun"
# [7,] "the huge"   "the" "huge"
# [8,] "the ball"   "the" "ball"
# [9,] "the woman"  "the" "woman"
# [10,] "a helps"    "a"   "helps"

# Replacing matches
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
# [1] "-pple"  "p-ar"   "b-nana"

str_replace_all(x, "[aeiou]", "-")
# [1] "-ppl-"  "p--r"   "b-n-n-"

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))
# [1] "one house"    "two cars"     "three people"

sentences %>%
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>%
  head(3)
# [1] "The canoe birch slid on the smooth planks."
# [2] "Glue sheet the to the dark blue background."
# [3] "It's to easy tell the depth of a well."

# Splitting
sentences %>%
  head(3) %>%
  str_split(" ")
# [[1]]
# [1] "The"     "birch"   "canoe"   "slid"    "on"      "the"     "smooth"
# [8] "planks."
#
# [[2]]
# [1] "Glue"        "the"         "sheet"       "to"          "the"
# [6] "dark"        "blue"        "background."
#
# [[3]]
# [1] "It's"  "easy"  "to"    "tell"  "the"   "depth" "of"    "a"     "well."

"a|b|c|d" %>%
  str_split("\\|") %>%
  .[[1]]
# [1] "a" "b" "c" "d"

sentences %>%
  head(5) %>%
  str_split(" ", simplify = TRUE)
#     [,1]    [,2]    [,3]    [,4]      [,5]  [,6]    [,7]     [,8]
# [1,] "The"   "birch" "canoe" "slid"    "on"  "the"   "smooth" "planks."
# [2,] "Glue"  "the"   "sheet" "to"      "the" "dark"  "blue"   "background."
# [3,] "It's"  "easy"  "to"    "tell"    "the" "depth" "of"     "a"
# [4,] "These" "days"  "a"     "chicken" "leg" "is"    "a"      "rare"
# [5,] "Rice"  "is"    "often" "served"  "in"  "round" "bowls." ""
#     [,9]
# [1,] ""
# [2,] ""
# [3,] "well."
# [4,] "dish."
# [5,] ""

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>%
  str_split(": ", n = 2, simplify = TRUE)
#     [,1]      [,2]
# [1,] "Name"    "Hadley"
# [2,] "Country" "NZ"
# [3,] "Age"     "35"

# Others
i <- c("I", "İ", "i", "ı")
str_subset(i, coll("i", ignore_case = TRUE))
# [1] "I" "i"

x <- "This is a sentence."
str_extract_all(x, boundary("word"))
# [[1]]
# [1] "This"     "is"       "a"        "sentence"
